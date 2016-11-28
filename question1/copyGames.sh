pgnFile=$1;
n=$2;
nthGame=$(grep -n "\[Event " $pgnFile | head -n $n | tail -n 1 | cut -d: -f 1)

lastLine=$(cat -n $pgnFile | tail -n +$(( $nthGame+1 )) | \
	perl -lane 'if ($_ =~ /^[\r\s]*(\d+)[\s\r]*$/) {print("$1");}' | head -n2 | tail -n1)


head -n $lastLine $pgnFile

