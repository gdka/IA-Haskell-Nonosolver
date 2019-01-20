#!/bin/bash
mkdir -p res/{cnf,res,pbm,minisatOutput}
for pathname in nonogramas/*.non; do
  echo "Solving $pathname..."
  "./proyecto-3/nonosolver" -non "$pathname" \
    -cnf "res/cnf/$(basename $pathname .non).cnf" \
    -res "res/res/$(basename $pathname .non).res" \
    -pbm "res/pbm/$(basename $pathname .non).pbm" \
    -x 10 > "res/minisatOutput/$(basename $pathname .non).log"
done
