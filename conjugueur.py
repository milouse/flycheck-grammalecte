#!/usr/bin/env python3
"""conjugueur.py

This script display the french conjugation of known verb to
Grammalecte.  It requires the grammalecte package.

Copyright (C) 2018+ Étienne Deparis <etienne@depar.is>
This is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.
"""

import sys
from grammalecte.fr.conj import Verb


def print_forms(data, forms):
    for p in forms:
        if p in data and data[p] != "":
            print("- " + data[p])


if __name__ == "__main__":
    if len(sys.argv) != 2:
        sys.exit(1)
    try:
        v = Verb(sys.argv[1])
    except (TypeError, ValueError):
        print("Verbe non trouvé")
        sys.exit()

    if v.sVerb in ["être", "avoir"]:
        print("* {} · {}".format(v.sVerb, v.sInfo))
    else:
        print("* {} · verbe du {}".format(v.sVerb, v.sInfo))
    print()

    for k, data in v.dConj.items():
        if k == ":Y":
            continue
        print("** " + data["label"] + "\n")
        if k == ":P":
            if k in data:
                # Newer version of Grammalecte (2.1.1+)
                print(data[k])
            else:
                # Older version
                print(data[":"])
        if k == ":Q":
            print("À conjuguer avec l’auxiliaire *{}* :\n".format(v.sVerbAux))
            if ":m:s" in data:
                # Newer version of Grammalecte (2.1.1+)
                print_forms(data, [":f:s", ":f:p", ":m:s", ":m:p"])
            else:
                # Older version
                print_forms(data, [":Q3", ":Q4", ":Q1", ":Q2"])
        else:
            print_forms(data, [":1s", ":2s", ":3s", ":1p", ":2p", ":3p"])
        print()
