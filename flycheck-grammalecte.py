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

import re
import fileinput
import grammalecte
import grammalecte.text as txt
from argparse import ArgumentParser


def main(files, opts={}):
    """Read the file and run grammalecte on it"""

    # Read input from stdin or first arg.
    text_input = []
    # At which list the real document begins?
    document_offset = 0
    for line in fileinput.input(files=files):
        borders = ["^--text follows this line--",
                   "^\begin{document}"]
        border_match = False
        for b in borders:
            if re.search(b, line, re.I):
                border_match = True
                break
        if border_match:
            # Discard all previous lines, which are considered as
            # headers in some mode (latex, mail...)
            document_offset = len(text_input) + 1
            text_input = []
            continue
        text_input.append(line)
    text, lineset = txt.createParagraphWithLines(
        list(enumerate(text_input)))

    # Replace filters patterns with some character to preserve position
    replacement_char = "&"
    filters = opts.get("filters", [])
    for pattern in filters:
        p = re.compile(pattern)
        for i in p.finditer(text):
            beg = text[:i.start()]
            end = text[i.end():]
            repl = (i.end() - i.start()) * replacement_char
            text = beg + repl + end

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

        gramm_err = gc.gce.parse(
            text, "FR",
            bDebug=False)

    if do_spell:
        spell_err = gc.oSpellChecker.parseParagraph(text, False)

    # Get colums and lines.
    gramm_err, spell_err = txt.convertToXY(gramm_err, spell_err, lineset)

    org_keywords = [
        "author", "caption", "category", "creator", "date", "email",
        "header", "keywords", "language", "name", "options", "title",
        "attr_.+"
    ]
    org_re = re.compile(
        r"^#\+(?:{})\:".format("|".join(org_keywords)),
        re.IGNORECASE)

    # Output
    if do_gramm:
        for i in list(gramm_err):
            cur_line = text_input[i["nStartY"]]
            if i["sType"] == "esp":
                # Remove useless space warning for visual paragraph in
                # text modes
                next_line_no = i["nStartY"] + 1
                if next_line_no > len(text_input):
                    # Weird, but maybe there is no blank line at the end
                    # of the file? Or some sort of buffer overflow?
                    next_line = ""
                else:
                    next_line = text_input[next_line_no].strip()
                if cur_line[i["nStartX"]] == "\n" and next_line == "":
                    continue
            elif i["sType"] == "nbsp":
                # Remove some unwanted nbsp warnings
                if cur_line[0:4] == "#-*-":
                    continue
                if org_re.search(cur_line) is not None:
                    continue
            message = i["sMessage"]
            suggs = i.get("aSuggestions", [])
            if len(suggs) > 0:
                message += " ⇨ " + ", ".join(suggs)
            message = message.replace("“", "« ").replace("« ", "« ") \
                             .replace("”", " »").replace(" »", " »")
            print("grammaire|{}|{}|{}"
                  .format(i["nStartY"] + 1 + document_offset,
                          i["nStartX"] + 1,
                          message))

    if do_spell:
        for i in list(spell_err):
            cur_line = text_input[i["nStartY"]]
            if org_re.search(cur_line) is not None \
               and i["sValue"] in org_keywords:
                continue
            print("orthographe|{}|{}|{}"
                  .format(i["nStartY"] + 1 + document_offset,
                          i["nStartX"] + 1,
                          "« {} » absent du dictionnaire".format(i["sValue"])))


if __name__ == "__main__":
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
    parser.add_argument('files', metavar='FILE', nargs='*',
                        help="files to read, if empty, stdin is used")

    args = parser.parse_args()
    # By default, fileinput will take all ARGV args. We need to filter
    # files now.
    files = args.files if len(args.files) > 0 else ('-', )
    opts = {
        "no_spell": args.no_spellcheck,
        "no_gramm": args.no_grammar,
        "no_apos": args.no_apostrophe,
        "no_nbsp": args.no_nbsp,
        "no_esp": args.no_space,
        "filters": args.filters
    }
    main(files, opts)
