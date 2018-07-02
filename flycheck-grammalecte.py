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

import fileinput
import grammalecte
import grammalecte.text as txt
from argparse import ArgumentParser


def main(files, no_spell=False, no_gramm=False):
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

    # Output
    if not no_gramm:
        for i in list(gramm_err):
            print("grammaire|{}|{}|{}\n"
                  .format(i["nStartY"]+1, i["nStartX"]+1,
                          i["sMessage"]))
    if not no_spell:
        for i in list(spell_err):
            print("orthographe|{}|{}|{}\n"
                  .format(i["nStartY"]+1, i["nStartX"]+1,
                          "Mot absent du dictionnaire"))


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("-S", "--no-spellcheck", action="store_true",
                        help="Don't report spellcheck errors")
    parser.add_argument("-G", "--no-grammar", action="store_true",
                        help="Don't report grammar errors")
    parser.add_argument('files', metavar='FILE', nargs='*',
                        help="files to read, if empty, stdin is used")

    args = parser.parse_args()
    # By default, fileinput will take all ARGV args. We need to filter
    # files now.
    files = args.files if len(args.files) > 0 else ('-', )
    main(files, no_spell=args.no_spellcheck, no_gramm=args.no_grammar)
