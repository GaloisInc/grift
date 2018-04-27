#!/bin/bash

find ./src/ -name '*.hs' | xargs graphmod -q > mods.dot
/usr/local/Cellar/graphviz/2.40.1/bin/tred mods.dot > mods2.dot
/usr/local/Cellar/graphviz/2.40.1/bin/dot -Tpdf mods2.dot > mods.pdf
