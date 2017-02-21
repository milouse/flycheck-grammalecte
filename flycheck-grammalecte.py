#!/usr/bin/env python3
import fileinput

import grammalecte.fr as gce
import grammalecte.fr.lexicographe as lxg
import grammalecte.fr.textformatter as tf
import grammalecte.text as txt
import grammalecte.tokenizer as tkz
from grammalecte.echo import echo

spell = True
gram = True

text = [line for line in fileinput.input()]
s = ''
gce.load()

sText, lLineSet = txt.createParagraphWithLines(list(enumerate(text)))
oDict = gce.getDictionary()
oTokenizer = tkz.Tokenizer("fr")

aGrammErrs = []
aSpellErrs = []

if gram:
    aGrammErrs = gce.parse(sText, "FR", bDebug=False, bContext=True)
if spell:
    for dToken in oTokenizer.genTokens(sText):
        if dToken['sType'] == "WORD" and not oDict.isValidToken(dToken['sValue']):
            aSpellErrs.append(dToken)
aGrammErrs, aSpellErrs = txt.convertToXY(aGrammErrs, aSpellErrs, lLineSet)
for i in list(aGrammErrs):
    s += 'grammaire|{}|{}|{}\n'.format(i['nStartY']+1,i['nStartX']+1, i['sMessage'])
for i in list(aSpellErrs):
    s += 'orthographe|{}|{}|{}\n'.format(i['nStartY']+1,i['nStartX']+1, 'Orthographe')
print(s)    
