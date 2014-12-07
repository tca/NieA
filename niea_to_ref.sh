#!/bin/bash
export file_in=$1
export file_out="${file_in/%.niea/.ref}"
rlwrap csi pat.scm sets.scm builtins.scm c-expr.scm scope.scm cc.scm hoist.scm niea.scm -e '(import niea)' -e "(validate-program \"$file_in\")" > "$file_out"
