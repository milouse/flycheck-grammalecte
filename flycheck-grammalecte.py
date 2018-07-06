#!/usr/bin/env python3
"""flycheck-grammalecte.py

This script is the glue that link flycheck syntax checker for emacs
and the grammalecte http://www.dicollecte.org/grammalecte/ syntax
checker. It requires the grammalecte package.

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
    # Load grammalecte.
    oGrammarChecker = grammalecte.GrammarChecker("fr")

    # Read input from stdin or first arg.
    text_input = [line for line in fileinput.input(files=files)]
    text, lineset = txt.createParagraphWithLines(list(enumerate(text_input)))

    # Grammar errors
    gramm_err, spell_err = oGrammarChecker.getParagraphErrors(
        text, bDebug=False)

    # Get colums and lines.
    gramm_err, spell_err = txt.convertToXY(gramm_err, spell_err, lineset)

    org_keywords = [
        "author", "caption", "category", "creator", "date", "email",
        "header", "keywords", "language", "name", "options", "title",
        "attr_.+"
    ]

    # Output
    if "no_gramm" not in opts or opts["no_gramm"] is False:
        org_re = re.compile(
            "^#\\+(?:{})\\:$".format("|".join(org_keywords)),
            re.IGNORECASE)
        for i in list(gramm_err):
            next_line_no = i["nStartY"] + 1
            next_char_no = i["nStartX"] + 1
            if i["sType"] == "apos" and "no_apos" in opts and \
               opts["no_apos"] is True:
                continue
            elif i["sType"] == "esp":
                cur_line = text_input[i["nStartY"]]
                next_line = text_input[next_line_no]
                if cur_line[i["nStartX"]] == "\n" and \
                   next_line.strip() == "":
                    continue
            elif i["sType"] == "nbsp":
                # If the option is set, directly pass to the following
                # error
                if "no_nbsp" in opts and opts["no_nbsp"] is True:
                    continue
                # Else, remove some unwanted nbsp warnings
                cur_line = text_input[i["nStartY"]]
                if cur_line[0:4] == "#-*-":
                    continue
                m = org_re.match(cur_line[0:next_char_no])
                if m is not None and m.start() == 0:
                    continue
            print("grammaire|{}|{}|{}\n"
                  .format(next_line_no, i["nStartX"] + 1,
                          i["sMessage"]))

    if "no_spell" not in opts or opts["no_spell"] is False:
        for i in list(spell_err):
            cur_line = text_input[i["nStartY"]]
            next_char_no = i["nStartX"] + 1
            org_re = re.compile(
                "(?:{})\\:".format("|".join(org_keywords)),
                re.IGNORECASE)
            m = org_re.match(cur_line, i["nStartX"])
            if m is not None and m.start() == i["nStartX"]:
                continue
            print("orthographe|{}|{}|{}\n"
                  .format(i["nStartY"] + 1, i["nStartX"] + 1,
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
        "no_nbsp": args.no_nbsp
    }
    main(files, opts)
