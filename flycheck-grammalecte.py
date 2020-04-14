#!/usr/bin/env python3
"""flycheck-grammalecte.py

This script is the glue that link flycheck syntax checker for emacs
and the grammalecte http://www.dicollecte.org/grammalecte/ syntax
checker.  It requires the grammalecte package.

Copyright (C) 2018+ Étienne Deparis <etienne@depar.is>
Copyright (C) 2017 Guilhem Doulcier <guilhem.doulcier@espci.fr>
This is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.
"""

import os
import re
import sys
import grammalecte
import grammalecte.text as txt
from argparse import ArgumentParser
from operator import itemgetter


def debug(msg):
    if not os.path.exists("debug"):
        return
    with open("debug", "a") as f:
        f.write(msg + "\n")


def _split_input(input_file):
    # At which line the real document begins?
    offset = 0
    text_input = []
    with open(input_file, "r") as f:
        input_lines = f.readlines()
    for line in input_lines:
        borders = ["^--text follows this line--",
                   r"^\\begin{document}"]
        border_match = False
        for b in borders:
            if re.search(b, line, re.I):
                border_match = True
                break
        if border_match:
            # Discard all previous lines, which are considered as
            # headers in some mode (latex, mail...)
            offset = len(text_input) + 1
            text_input = []
            continue
        text_input.append(line)
    return offset, text_input


def _mask_matching_pattern(new_text, start, span, repl):
    before = new_text[:start]
    after = new_text[span[0]:]
    mask = (span[0] - start) * repl
    return (span[1], before + mask + after)


def _redact_text(pattern, text, repl="█"):
    new_text = text
    for m in pattern.finditer(text):
        nl_count = len(m[0].splitlines()) - 1
        line_pad = "\n" * nl_count
        group_nb = len(m.groups())
        if group_nb == 0:
            mask = (repl * len(m[0])) + line_pad
            new_text = pattern.sub(mask, new_text, count=1)
            continue
        group_nb += 1  # Because matching groups are indexed from 1
        beg = m.start()
        for i in range(1, group_nb):
            if m.group(i) is None:
                continue
            beg, new_text = _mask_matching_pattern(
                new_text, beg, m.span(i), repl
            )
        new_text = _mask_matching_pattern(
            new_text, beg, (m.end(), m.end()), repl
        )[1] + line_pad
    return new_text


def _prepare_gramm_errors(gramm_err, document_offset, text_input):
    final_errors = []
    for i in list(gramm_err):
        cur_line = text_input[i["nStartY"]]
        cur_line_nb = i["nStartY"] + 1
        if i["sType"] == "esp":
            # Remove useless space warning for visual paragraph in
            # text modes
            if cur_line_nb > len(text_input):
                # Weird, but maybe there is no blank line at the end
                # of the file? Or some sort of buffer overflow?
                next_line = ""
            else:
                # cur_line_nb hold the human value of the current
                # line, starting from 1. Thus this human value
                # equals the next line index, starting from 0.
                next_line = text_input[cur_line_nb].strip()
            if i["nStartX"] == len(cur_line) and next_line == "":
                continue
        message = i["sMessage"]
        suggs = i.get("aSuggestions", [])
        if len(suggs) > 0:
            message += " ⇨ " + ", ".join(suggs)
        message = message.replace("“", "« ").replace("« ", "« ") \
                         .replace("”", " »").replace(" »", " »")
        cur_col = i["nStartX"] + 1
        real_line_nb = cur_line_nb + document_offset
        final_errors.append(
            ("grammaire", real_line_nb, cur_col, message)
        )
    return final_errors


def _prepare_spell_errors(spell_err, document_offset):
    final_errors = []
    for i in list(spell_err):
        real_line_nb = i["nStartY"] + 1 + document_offset
        cur_col = i["nStartX"] + 1
        err_msg = "« {} » absent du dictionnaire".format(i["sValue"])
        final_errors.append(("orthographe", real_line_nb, cur_col, err_msg))
    return final_errors


def find_errors(input_file, opts={}):
    """Read the file and run grammalecte on it"""

    document_offset, text_input = _split_input(input_file)

    # Cleanup text by redacting all matching patterns.
    # Each line of text already end by a end-line marker, thus only join
    # by empty string
    raw_text = "".join(text_input)
    for pattern in opts.get("filters", []):
        raw_text = _redact_text(re.compile(pattern), raw_text)
    debug(raw_text)
    # Converting back text to list of lines
    text_input = raw_text.splitlines()

    text, lineset = txt.createParagraphWithLines(
        list(enumerate(text_input))
    )

    do_gramm = not opts.get("no_gramm", False)
    do_spell = not opts.get("no_spell", False)
    gramm_err = spell_err = []

    # Load grammalecte.
    gc = grammalecte.GrammarChecker("fr")

    # Compute grammar and spell check errors
    if do_gramm:
        gc.gce.setOption("apos", not opts.get("no_apos", False))
        gc.gce.setOption("nbsp", not opts.get("no_nbsp", False))
        gc.gce.setOption("esp", not opts.get("no_esp", False))
        gc.gce.setOption("tab", not opts.get("no_esp", False))
        gramm_err = gc.gce.parse(text, "FR", bDebug=False)

    if do_spell:
        spell_err = gc.oSpellChecker.parseParagraph(text, False)

    # Get colums and lines.
    gramm_err, spell_err = txt.convertToXY(gramm_err, spell_err, lineset)

    if do_gramm:
        final_errors = _prepare_gramm_errors(
            gramm_err, document_offset, text_input
        )
    else:
        final_errors = []

    if do_spell:
        final_errors += _prepare_spell_errors(spell_err, document_offset)

    return sorted(final_errors, key=itemgetter(1, 2))


if __name__ == "__main__":
    debug(sys.argv.__repr__())

    parser = ArgumentParser()
    parser.add_argument("-S", "--no-spellcheck", action="store_true",
                        help="Don't report spellcheck errors")
    parser.add_argument("-G", "--no-grammar", action="store_true",
                        help="Don't report grammar errors")
    parser.add_argument("-A", "--no-apostrophe", action="store_true",
                        help="Don't report apostrophe errors")
    parser.add_argument("-N", "--no-nbsp", action="store_true",
                        help="Don't report non-breakable spaces errors")
    parser.add_argument("-W", "--no-space", action="store_true",
                        help="Don't report useless spaces and tabs errors")
    parser.add_argument('-f', "--filters", action="append", default=[],
                        help="Filter pattern (regular expression "
                        "replaced before analysis)")
    parser.add_argument('file', help="File to proofed")

    args = parser.parse_args()
    opts = {
        "no_spell": args.no_spellcheck,
        "no_gramm": args.no_grammar,
        "no_apos": args.no_apostrophe,
        "no_nbsp": args.no_nbsp,
        "no_esp": args.no_space,
        "filters": args.filters
    }
    errors = find_errors(args.file, opts)
    for err in errors:
        msg = "{}|{}|{}|{}".format(*err)
        debug(msg)
        print(msg)
