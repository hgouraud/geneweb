#!/bin/sh
#set -ex
usage()
{
echo "Usage: $cmd <database name> [wizard_id:passwd]
Hardcoded crl tests on input geneweb <database name>
and ultimately check that no failures in $GWDLOG
Need to properly set 'hardcoded vars' in script header.
"
exit 1
}

BASE=$1 #<database name>
PWD=$2  #[wizard_id:passwd] (some commands require wizard priviledge)

# assumes we are running in the repo folder
# ./test/run-GW-test.sh

BASES_DIR="$HOME/Genea/GeneWeb-Bases"
DIST_DIR="./distribution"
BIN_DIR="$DIST_DIR/gw"
GWDLOG=./distribution/gw/gwd.log

#=== hardcoded vars (start) ===

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
NOTE="chantal" # one specific note
#=== hardcoded vars (end)   ===

#===  main ====================
cmd=$(basename $0)
test "$1" || usage

# this assumes a fresh (empty) gwd.log file
rm -f $GWDLOG
killall gwd

OCAMLRUNPARAM=b "$BIN_DIR"/gwd \
  -setup_link \
  -bd "$BASES_DIR" \
  -hd "$BIN_DIR" \
  -add_lexicon "$BASES_DIR"/lang/lexicon-hg.txt \
  -allowed_tags "$BASES_DIR"/tags.txt \
  -trace_failed_passwd \
  -robot_xcl 10000,1 \
  -conn_tmout 3600 \
  -blang \
  -log "<stderr>" \
  -plugins -unsafe "$BIN_DIR"/plugins \
  2>> "$GWDLOG" &

# give some time for gwd to start
sleep 1

if pgrep "gwd" > /dev/null
then
  echo "Running GW-test"
else
  echo "gwd not running" \
  exit
fi

curlopt='-sS -o /dev/null'
crl () {
  local cmd=$1
  curl $curlopt "http://localhost:2317/$BASE?w=$PWD&$cmd"
  if [ $? -ne 0 ]; then
    echo "Failed to execute $cmd."
  fi
}

crl "m=CAL"
crl "m=NOTES"
crl "m=MISC_NOTES"
crl "m=NOTES&f=$NOTE"
crl "m=WIZNOTES"
crl "m=STAT"
crl "m=LB&k=30"
crl "m=LD&k=30"
crl "m=LM&k=30"
crl "m=OE&k=30"
crl "m=OA&k=30"
crl "m=LL&k=30"
crl "m=ANM"
crl "m=AN"
crl "m=AD"
crl "m=AM"
crl "m=HIST&k=20"
crl "m=FORUM"
#crl "m=FORUM&p=939" # too base specific
crl "m=FORUM_ADD"
crl "m=ADD_FAM"
crl "m=PPS&bi=on&ba=on&ma=on&de=on&bu=on"
crl "m=AS"
crl "m=H&v=conf"
crl "m=MOD_DATA&data=fn"
crl "m=MOD_DATA&data=sn"
crl "m=MOD_DATA&data=place"
crl "m=MOD_DATA&data=occu"
crl "m=MOD_DATA&data=src"
crl "m=TT"
crl "m=TT&p=*"
crl "m=N&tri=A"
crl "m=N&tri=F"
crl "m=P&tri=A"
crl "m=P&tri=F"

#les lignes qui suivent doivent être ajustées en fonction de votre base

crl "m=S&n=$FN+$SN&p="
crl "p=$FN&n=$SN&oc=$OC"
crl "p=$FN1&n=$SN1&oc=$OC1"
crl "p=$FN2&n=$SN2&oc=$OC2"
crl "p=xxx&n=yyy"

crl "m=MOD_IND&i=$ID"
crl "m=CHG_EVT_IND_ORD&i=$ID"
crl "m=SND_IMAGE&i=$ID"
crl "i=$ID&m=A"
crl "i=$ID&m=A&t=T&v=5"
crl "i=$ID&m=A&t=H&v=5"
crl "i=$ID&m=A&t=Z&v=6&maxv=19&num=on&birth=on&birth_place=on&marr=on&marr_date=on&marr_place=on&child=on&death=on&death_place=on&age=on&occu=on&repeat=on&gen=1&ns=1&hl=1"
crl "i=$ID&m=A&t=G&v=3&maxv=19&siblings=on&alias=on&parents=on&rel=on&witn=on&notes=on&src=on&hide=on"
crl "i=$ID&m=D"
crl "i=$ID&m=D&t=V&v=3"
crl "i=$ID&m=D&t=TV&v=3"
crl "i=$ID&m=D&t=V&v=3"
crl "i=$ID&m=D&t=I&v=3&num=on&birth=on&birth_place=on&marr=on&marr_date=on&marr_place=on&child=on&death=on&death_place=on&age=on&occu=on&gen=1&ns=1&hl=1"
crl "i=$ID&m=D&t=L&v=3&maxv=3&siblings=on&alias=on&parents=on&rel=on&witn=on&notes=on&src=on&hide=on"
crl "i=$ID&m=D&t=A&num=on&v=3"

crl "i=$ID&m=R"
crl "i=$ID&m=C&v=3"
crl "i=$ID&m=F"
crl "i=$ID&m=C&t=AN"
crl "i=$ID&m=C"

crl "m=F&p=odette&n=loubry"

# assumes no error in generated gwd.log
set +ex
echo "$GWDLOG reported traces (empty if no failure):"
grep "CRITICAL" $GWDLOG
grep "ERROR" $GWDLOG
grep "WARNING" $GWDLOG
grep "Failed" $GWDLOG
