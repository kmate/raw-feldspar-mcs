./build.sh $1 "$2"
e-reset
./run.sh $1
e-reset
./run.sh $1
e-reset
./run.sh $1
./bdiff.sh expected.dat output.dat

