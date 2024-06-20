
# you may add --rewrite flag sometimes
# and maybe --comp-opts=all
# and maybe --polyMixed
# --subdiv \
# --divLimit=2 --totalOpt=102400 \
./daisy --rangeMethod=affine --errorMethod=affine --analysis=dataflow \
 --precision=Fixed64 \
 --mixed-tuning \
"testcases/Minv/MinvPanda.scala"
