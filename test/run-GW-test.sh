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
WIZ=hg
FN=henri
SN=gouraud
OC=0
ID=1711 # individual Id, ideally should have multiple events
FID=597 # family id for this individual, ideally, should have multiple families
IMG_C="alain.0.de_fouchier.jpg" # une image du carrousel de $ID!
IMG_C_S="alain.0.boucher.jpg" # une image sauvegardée du carrousel de $ID!
IMG_SRC="famille-h-gouraud.jpg" # une image dans bases/src/mabase/images
TXT_SRC="famille-h-gouraud.txt" # une source dans bases/src/mabase
IMG_IM="henri.0.gouraud.jpg" # un portrait dans bases/images/mabase
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

curlopt='-sS -o /tmp/tmp.txt'
crl () {
  local cmd=$1
  curl $curlopt "http://localhost:2317/$BASE?w=$PWD&$cmd"
  if [ $? -ne 0 ]; then
    echo "Failed to execute $cmd."
  fi
  if grep "<h1>Incorrect request</h1>" /tmp/tmp.txt; then
    echo "Incorrect request with $cmd."
  fi
}

crl "m=S&n=$FN+$SN&p="
crl "p=$FN&n=$SN&oc=$OC"
crl "p=$FN1&n=$SN1&oc=$OC1"
crl "p=$FN2&n=$SN2&oc=$OC2"
crl "p=xxx&n=yyy"

crl "m=A&i=$ID"
crl "m=A&i=$ID&t=T&v=5"
crl "m=A&i=$ID&t=H&v=5"
crl "m=A&i=$ID&t=Z&v=6&maxv=19&num=on&birth=on&birth_place=on&marr=on&marr_date=on&marr_place=on&child=on&death=on&death_place=on&age=on&occu=on&repeat=on&gen=1&ns=1&hl=1"
crl "m=A&i=$ID&t=G&v=3&maxv=19&siblings=on&alias=on&parents=on&rel=on&witn=on&notes=on&src=on&hide=on"
crl "m=AD"
crl "m=ADD_FAM"
crl "m=ADD_IND"
crl "m=ADD_PAR&pp=$FN2&np=$SN2&$ocp=$OC2"
crl "m=AM"
crl "m=AN"
crl "m=ANM"
crl "m=AS"
crl "m=C&i=$ID&v=3"
crl "m=C&i=$ID&t=AN"
crl "m=C&i=$ID"
crl "m=CAL"
crl "m=CHG_CHN&ip=$FID"
crl "m=CHG_EVT_FAM_ORD&i=$FID&ip=$ID"
crl "m=CHG_EVT_IND_ORD&i=$ID"
crl "m=CHG_FAM_ORD&f=$FID&i=$ID&n=2"
crl "m=CONN_WIZ"
crl "m=D&i=$ID"
crl "m=D&i=$ID&t=V&v=3"
crl "m=D&i=$ID&t=TV&v=3"
crl "m=D&i=$ID&t=V&v=3"
crl "m=D&i=$ID&t=I&v=3&num=on&birth=on&birth_place=on&marr=on&marr_date=on&marr_place=on&child=on&death=on&death_place=on&age=on&occu=on&gen=1&ns=1&hl=1"
crl "m=D&i=$ID&t=L&v=3&maxv=3&siblings=on&alias=on&parents=on&rel=on&witn=on&notes=on&src=on&hide=on"
crl "m=D&i=$ID&t=A&num=on&v=3"
crl "m=DEL_FAM&i=$FID&ip=$ID1"
crl "m=DEL_IND&i=$ID"
crl "m=DOC&s=$IMG_SRC"
crl "m=DOCH&s=$IMG_SRC"
crl "m=F&i=$ID"
crl "m=FORUM"
#crl "m=FORUM&p=939" # too base specific
crl "m=FORUM_ADD"
crl "m=H&v=conf"
crl "m=H&v=$TXT_SRC"
crl "m=HIST&k=20"
crl "m=HIST_CLEAN&i=$ID&f=$FN.$OC.$SN"
crl "m=HIST_DIFF&t=SUM&f=$FN.$OC.$SN"
crl "m=HIST_DIFF&t=SUM&f=$FN.$OC.$SN&new=0&old=1"
crl "m=HIST_SEARCH&i=$ID"
crl "m=IM&s=$IMG_SRC"
crl "m=IMH&s=$IMG_SRC"
crl "m=IM_C&i=$ID"
crl "m=IM_C_S&i=$ID"
crl "m=IM_C&i=$ID&s=$IMG_C"
crl "m=IM_C_S&i=$ID&s=$IMG_C_S"
crl "m=IM_C&i=$ID&s=$IMG_C" # TODO voir comportement si pas d'image sauvée
crl "m=INV_FAM&i=$ID&f=$FID" # f=family_id is base specific!
crl "m=L"
crl "m=LB&k=30"
crl "m=LD&k=30"
crl "m=LL&k=30"
crl "m=LM&k=30"
crl "m=MISC_NOTES"
crl "m=MOD_DATA&data=fn"
crl "m=MOD_DATA&data=sn"
crl "m=MOD_DATA&data=place"
crl "m=MOD_DATA&data=occu"
crl "m=MOD_DATA&data=src"
crl "m=MOD_NOTES&f=$NOTE"
crl "m=MOD_IND&i=$ID"
crl "m=MOD_WIZNOTES&f=$WIZ"
crl "m=MRG&i=$ID"
#crl "m=MRG_DUP"
#crl "m=MRG_DUP_IND_Y_N"
#crl "m=MRG_FAM"
#crl "m=MRG_DUP_FAM_Y_N"
crl "m=MRG_IND"
crl "m=N&tri=A"
crl "m=N&tri=F"
crl "m=NOTES"
crl "m=NOTES&f=$NOTE"
crl "m=OA&k=30"
crl "m=OE&k=30"
crl "m=P&tri=A"
crl "m=P&tri=F"
crl "m=PERSO&i=$ID"
crl "m=PS"
crl "m=PPS&bi=on&ba=on&ma=on&de=on&bu=on"
crl "m=R&i=$ID"
crl "m=REFRESH&i=$ID"
#crl "m=RL&i=$ID&i1" # m=RL&i=5316&l1=3&i1=1711&l2=2&i2=6223&dag=on
crl "m=RLM&i1=$ID&p2=$FN2&n2=$SN2&oc2=$OC2"
crl "m=SND_IMAGE&i=$ID"
crl "m=SND_IMAGE_C&i=$ID"
crl "m=SRC&v=$TXT_SRC"
crl "m=STAT"
crl "m=TT"
crl "m=TT&p=*"
crl "m=WIZNOTES"

# ATTENTION, les autres fonctions du carrousel (_OK) ont une action immédiate!!

echo "$GWDLOG reported traces (empty if no failure):"
grep "CRITICAL" $GWDLOG
grep "ERROR" $GWDLOG
grep "WARNING" $GWDLOG
grep "Failed" $GWDLOG

