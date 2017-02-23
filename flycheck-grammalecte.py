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

import grammalecte.fr as gce
import grammalecte.text as txt
import grammalecte.tokenizer as tkz

def main():
    '''Read the file and run grammalecte on it'''
    # Load grammalecte.
    gce.load()
    dictionary = gce.getDictionary()
    tokenizer = tkz.Tokenizer("fr")

    # Read input from stdin or first arg.
    text_input = [line for line in fileinput.input()]
    text, lineset = txt.createParagraphWithLines(list(enumerate(text_input)))

    # Grammar errors
    gramm_err = gce.parse(text, "FR", bDebug=False, bContext=True)

    # Spelling errors
    spell_err = []
    for token in tokenizer.genTokens(text):
        if token['sType'] == "WORD" and not dictionary.isValidToken(token['sValue']):
            spell_err.append(token)

    # Get colums and lines.
    gramm_err, spell_err = txt.convertToXY(gramm_err, spell_err, lineset)

    # Output
    for i in list(gramm_err):
        print('grammaire|{}|{}|{}\n'.format(i['nStartY']+1, i['nStartX']+1,
                                            i['sMessage']))
    for i in list(spell_err):
        print('orthographe|{}|{}|{}\n'.format(i['nStartY']+1, i['nStartX']+1,
                                              'Mot absent du dictionnaire'))

if __name__ == '__main__':
    main()
