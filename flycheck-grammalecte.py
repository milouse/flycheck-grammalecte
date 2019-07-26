#!/usr/bin/env python3
"""flycheck-grammalecte.py

This script is the glue that link flycheck syntax checker for emacs
and the grammalecte http://www.dicollecte.org/grammalecte/ syntax
checker. It requires the grammalecte package.

Copyright (C) 2018 Étienne Deparis <etienne@depar.is>
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

    do_gramm = ("no_gramm" not in opts or opts["no_gramm"] is False)
    do_spell = ("no_spell" not in opts or opts["no_spell"] is False)
    gramm_err = spell_err = []

    # Load grammalecte.
    gc = grammalecte.GrammarChecker("fr")

    # Compute grammar and spell check errors
    if do_gramm:
        gc.gce.setOption(
            "apos", "no_apos" not in opts or opts["no_apos"] is False)
        gc.gce.setOption(
            "nbsp", "no_nbsp" not in opts or opts["no_nbsp"] is False)
        gc.gce.setOption(
            "esp", "no_esp" not in opts or opts["no_esp"] is False)
        gc.gce.setOption(
            "tab", "no_esp" not in opts or opts["no_esp"] is False)

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

    # Output
    if do_gramm:
        org_re = re.compile(
            "^#\\+(?:{})\\:$".format("|".join(org_keywords)),
            re.IGNORECASE)
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
                # The following line is not subject to overflow
                # excepton, even if i["nStartX"] + 1 > len(cur_line)
                m = org_re.match(cur_line[0:i["nStartX"] + 1])
                if m is not None and m.start() == 0:
                    continue
            print("grammaire|{}|{}|{}\n"
                  .format(i["nStartY"] + 1 + document_offset,
                          i["nStartX"] + 1,
                          i["sMessage"]))

    if do_spell:
        for i in list(spell_err):
            cur_line = text_input[i["nStartY"]]
            org_re = re.compile(
                "(?:{})\\:".format("|".join(org_keywords)),
                re.IGNORECASE)
            m = org_re.match(cur_line, i["nStartX"])
            if m is not None and m.start() == i["nStartX"]:
                continue
            print("orthographe|{}|{}|{}\n"
                  .format(i["nStartY"] + 1 + document_offset,
                          i["nStartX"] + 1,
                          "Mot absent du dictionnaire"))


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
        "no_esp": args.no_space
    }
    main(files, opts)
