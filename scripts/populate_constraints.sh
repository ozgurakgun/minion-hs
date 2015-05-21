#!/bin/bash

minion help constraints \
    | grep '^help constraints' \
    | cut -d ' ' -f 3 \
    | grep -v "undefzero" \
    > populate_constraints.constraints

MINION_REPO=${HOME}/repos/stacs_cp/minion

cat ${MINION_REPO}/cmake-modules/constraints.cmake | grep '^set(NAME_READ_' \
    > populate_constraints.args

mkdir -p src/Language/Minion/Definition
runhaskell scripts/populate_constraints.hs > src/Language/Minion/Definition/Constraint.hs

rm populate_constraints.constraints populate_constraints.args

