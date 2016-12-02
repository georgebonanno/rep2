if [ $# -lt 1 ]; then
	echo "usage: $0 <pgn files>";
	exit 1;
fi
R -f chess.R "--args $@" 
