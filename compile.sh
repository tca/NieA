rlwrap csi pat.scm sets.scm c-expr.scm builtins.scm cc.scm hoist.scm gen-c.scm scope.scm niea.scm -e '(import niea)' -e '(runtime)' > out/runtime.h
rlwrap csi pat.scm sets.scm c-expr.scm builtins.scm cc.scm hoist.scm gen-c.scm scope.scm niea.scm -e '(import niea)' -e '(compile-program "nieatest/ufo0.niea")' > out/file.c
cd out && gcc file.c

