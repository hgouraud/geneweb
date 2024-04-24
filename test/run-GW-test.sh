#!/bin/sh
#set -ex
usage()
{
echo "Usage: $cmd <database name> [<'w=' parameter>]
Hardcoded curl tests on input geneweb <database name>
and ultimately check that no failures in $GWDLOG
Need to properly set 'hardcoded vars' in script header.
TODO: what is meaning of optionnal 'w=xx' parameter.
"
exit 1
}

# assumes we are running in the repo folder
# ./test/run-GW-test.sh
pwd

#=== hardcoded vars (start) ===

BASES_DIR="$HOME/Genea/GeneWeb-Bases"
DIST_DIR="./distribution"
BIN_DIR="$DIST_DIR/gw"

# this is the data for specific persons
# for synonym test there should be several occurrences of FN+SN
FN=henri
SN=gouraud
OC=0
ID=1711
# someone without grand parents
FN1=paul
OC1=1
SN1=cosse
# someone without parents
FN2=antoine
OC2=0
SN2=cosse

GWDLOG=$GW_DIR/distribution/gw/gwd.log
#=== hardcoded vars (end)   ===

#===  main ====================
cmd=$(basename $0)
test "$1" || usage
# this assumes a fresh (empty) gwd.log file

rm -f ./distribution/gw/gwd.log
touch ./distribution/gw/gwd.log

killall gwd

OCAMLRUNPARAM=b "$BIN_DIR"/gwd \
  -setup_link \
  -bd "$BASES_DIR" \
  -hd "$BIN_DIR" \
  -add_lexicon "$BASES_DIR/lang/lexicon-hg.txt \
  -allowed_tags "$BASES_DIR/tags.txt" \
  -trace_failed_passwd \
  -robot_xcl 10000,1 \
  -conn_tmout 3600 \
  -blang \
  -log "<stderr>" \
  -plugins -unsafe "$BIN_DIR"/plugins \
  > "$BIN_DIR"/gwd.log 2>&1 &

#rm -f $GWDLOG
touch $GWDLOG

curlopt='-sS -o /dev/null'

echo "Running GW-test"

curl $curlopt "http://localhost:2317/$1?m=CAL&w=$2"
if [ $? -ne 0 ]; then
    echo "Failed to execute m=CAL."
fi
curl $curlopt "http://localhost:2317/$1?m=NOTES&w=$2"
curl $curlopt "http://localhost:2317/$1?m=MISC_NOTES&w=$2"
curl $curlopt "http://localhost:2317/$1?m=NOTES&f=chantal&w=$2"
curl $curlopt "http://localhost:2317/$1?m=WIZNOTES&w=$2"
curl $curlopt "http://localhost:2317/$1?m=STAT&w=$2"
curl $curlopt "http://localhost:2317/$1?m=LB&k=30&w=$2"
curl $curlopt "http://localhost:2317/$1?m=LD&k=30&w=$2"
curl $curlopt "http://localhost:2317/$1?m=LM&k=30&w=$2"
curl $curlopt "http://localhost:2317/$1?m=OE&k=30&w=$2"
curl $curlopt "http://localhost:2317/$1?m=OA&k=30&w=$2"
curl $curlopt "http://localhost:2317/$1?m=LL&k=30&w=$2"
curl $curlopt "http://localhost:2317/$1?m=ANM&w=$2"
curl $curlopt "http://localhost:2317/$1?m=AN&w=$2"
curl $curlopt "http://localhost:2317/$1?m=AD&w=$2"
curl $curlopt "http://localhost:2317/$1?m=AM&w=$2"
curl $curlopt "http://localhost:2317/$1?m=HIST&k=20&w=$2"
curl $curlopt "http://localhost:2317/$1?m=FORUM&w=$2"
curl $curlopt "http://localhost:2317/$1?m=FORUM&p=939&w=$2"
curl $curlopt "http://localhost:2317/$1?m=FORUM_ADD&w=$2"
curl $curlopt "http://localhost:2317/$1?m=ADD_FAM&w=$2"
curl $curlopt "http://localhost:2317/$1?m=PPS&bi=on&ba=on&ma=on&de=on&bu=on&w=$2"
curl $curlopt "http://localhost:2317/$1?m=AS&w=$2"
curl $curlopt "http://localhost:2317/$1?m=H&v=conf&w=$2"
curl $curlopt "http://localhost:2317/$1?m=MOD_DATA&data=fn&w=$2"
curl $curlopt "http://localhost:2317/$1?m=MOD_DATA&data=sn&w=$2"
curl $curlopt "http://localhost:2317/$1?m=MOD_DATA&data=place&w=$2"
curl $curlopt "http://localhost:2317/$1?m=MOD_DATA&data=occu&w=$2"
curl $curlopt "http://localhost:2317/$1?m=MOD_DATA&data=src&w=$2"

curl $curlopt "http://localhost:2317/$1?m=TT&w=$2"
curl $curlopt "http://localhost:2317/$1?m=TT&p=*&w=$2"
curl $curlopt "http://localhost:2317/$1?m=N&tri=A&w=$2"
curl $curlopt "http://localhost:2317/$1?m=N&tri=F&w=$2"
curl $curlopt "http://localhost:2317/$1?m=P&tri=A&w=$2"
curl $curlopt "http://localhost:2317/$1?m=P&tri=F&w=$2"

#les lignes qui suivent doivent être ajustées en fonction de votre base

curl $curlopt "http://127.0.0.1:2317/$1?m=S&n=$FN+$SN&p=&w=$2"
curl $curlopt "http://127.0.0.1:2317/$1?p=$FN&n=$SN&oc=$OC&w=$2"
curl $curlopt "http://localhost:2317/$1?p=$FN1&n=$SN1&oc=$OC1&w=$2"
curl $curlopt "http://localhost:2317/$1?p=$FN2&n=$SN2&oc=$OC2&w=$2"
curl $curlopt "http://localhost:2317/$1?p=xxx&n=yyy&w=$2"

curl $curlopt "http://localhost:2317/$1?m=MOD_IND&i=$ID&w=$2"
curl $curlopt "http://localhost:2317/$1?m=CHG_EVT_IND_ORD&i=$ID&w=$2"
curl $curlopt "http://localhost:2317/$1?m=SND_IMAGE&i=$ID&w=$2"
curl $curlopt "http://localhost:2317/$1?i=$ID&m=A&w=$2"
curl $curlopt "http://localhost:2317/$1?i=$ID&m=A&t=T&v=5&w=$2"
curl $curlopt "http://localhost:2317/$1?i=$ID&m=A&t=H&v=5&w=$2"
curl $curlopt "http://localhost:2317/$1?i=$ID&m=A&t=Z&v=6&maxv=19&num=on&birth=on&birth_place=on&marr=on&marr_date=on&marr_place=on&child=on&death=on&death_place=on&age=on&occu=on&repeat=on&gen=1&ns=1&hl=1&w=$2"
curl $curlopt "http://localhost:2317/$1?i=$ID&m=A&t=G&v=3&maxv=19&siblings=on&alias=on&parents=on&rel=on&witn=on&notes=on&src=on&hide=on&w=$2"
curl $curlopt "http://localhost:2317/$1?i=$ID&m=D&w=$2"
curl $curlopt "http://localhost:2317/$1?i=$ID&m=D&t=V&v=3&w=$2"
curl $curlopt "http://localhost:2317/$1?i=$ID&m=D&t=TV&v=3&w=$2"
curl $curlopt "http://localhost:2317/$1?i=$ID&m=D&t=V&v=3&w=$2"
curl $curlopt "http://localhost:2317/$1?i=$ID&m=D&t=I&v=3&num=on&birth=on&birth_place=on&marr=on&marr_date=on&marr_place=on&child=on&death=on&death_place=on&age=on&occu=on&gen=1&ns=1&hl=1&w=$2"
curl $curlopt "http://localhost:2317/$1?i=$ID&m=D&t=L&v=3&maxv=3&siblings=on&alias=on&parents=on&rel=on&witn=on&notes=on&src=on&hide=on&w=$2"
curl $curlopt "http://localhost:2317/$1?i=$ID&m=D&t=A&num=on&v=3&w=$2"

curl $curlopt "http://localhost:2317/$1?i=$ID&m=R&w=$2"
curl $curlopt "http://localhost:2317/$1?i=$ID&m=C&v=3&w=$2"
curl $curlopt "http://localhost:2317/$1?i=$ID&m=F&w=$2"
curl $curlopt "http://localhost:2317/$1?i=$ID&m=C&t=AN&w=$2"
curl $curlopt "http://localhost:2317/$1?i=$ID&m=C&w=$2"

curl $curlopt "http://localhost:2317/$1?m=F&p=odette&n=loubry"

# assumes no error in generated gwd.log
set +ex
echo "$GWDLOG reported traces (empty if no failure):"
grep "CRITICAL" $GWDLOG
grep "ERROR" $GWDLOG
grep "WARNING" $GWDLOG
grep "Failed" $GWDLOG
