#!/bin/sh
BASE="test"
SOURCE_DIR="./test"
BASES_DIR="./distribution/bases"
BIN_DIR="./distribution/gw"
SERVER="http://localhost:2317"
TMP_DIR="./test"

test_comm ()
{
NL=$((`wc $1 | cut -c1-8`))
if [[ $NL -gt 40 ]]
then
  grep "Error:" $1 | head -20
  echo "........."
  grep "Error:" $1 | tail -20
else
  grep "Error:" $1
fi
echo "Nb lines (comm) = $NL"
}

test_diff ()
{
NL=$((`wc $1 | cut -c1-8`))
if [[ $NL -gt 40 ]]
then
  head -20 $1
  echo "........."
  tail -20 $1
else
  cat $1
fi
echo "Nn lines (diff) = $NL"
}

cp $SOURCE_DIR/$BASE.gw $BASES_DIR/$BASE-00.gw
cp $SOURCE_DIR/$BASE-00.gwf $BASES_DIR/$BASE-00.gwf

$BIN_DIR/gwc -nofail -f $BASES_DIR/$BASE-00.gw -o $BASES_DIR/$BASE-00 > $BASES_DIR/comm.log
echo "$BASE-00 created"
test_comm $BASES_DIR/comm.log
echo "Gwc to $BASE-00.gwb"

echo "Progression is achieved through several steps of the type:"
echo "Save .gw file to nn+1: gwu test-nn to test-nn+1.gw"
echo "Build base from nn+1: gwc test-nn+1.gw to test.nn+1.gwb"
echo ""

read -p "Suite? " suite
echo "******************************************************************"

$BIN_DIR/gwu $BASES_DIR/$BASE-00 -gwplus -o $BASES_DIR/$BASE-01.gw > $BASES_DIR/comm.log
echo ""
test_comm $BASES_DIR/comm.log
echo "Gwu $BASE-00.gwb to $BASE-01.gw"
diff $BASES_DIR/$BASE-00.gw $BASES_DIR/$BASE-01.gw > $TMP_DIR/tmp.txt
test_diff $TMP_DIR/tmp.txt
echo "diff $BASE-00.gw $BASE-01.gw."
echo "Should be the same (-gwplus mode) (nsck issue pending)."

read -p "Suite? " suite
echo "******************************************************************"

killall gwd
OCAMLRUNPARAM=b "$BIN_DIR"/gwd \
  -setup_link \
  -bd "$BASES_DIR" \
  -hd "$BIN_DIR" \
  -hd "$BASES_DIR" \
  -trace_templ \
  -robot_xcl 10000,1 \
  -log_level 7 \
  -unsafe_plugins "$BIN_DIR"/plugins \
  -no_digest \
  -log "<stderr>" \
  > "$BIN_DIR"/gwd.log 2>&1 &

sleep 2

rm $TMP_DIR/tmp.txt
#curl "$SERVER/$BASE-00?i=0" > $TMP_DIR/tmp.txt
#echo "$SERVER/$BASE-00?digest=&ip=0&i=0&m=MOD_FAM_OK&pa1_fn=Philippe&pa1_occ=0&pa1_sn=Test&pa1_p=link&pa1b_dd=&pa1b_mm=&pa1b_yyyy=&pa1b_pl=&pa1d_dd=&pa1d_mm=&pa1d_yyyy=&pa1d_pl=&pa1_occupation=&pa2_fn=Janine&pa2_occ=0&pa2_sn=Durand&pa2_p=link&pa2b_dd=&pa2b_mm=&pa2b_yyyy=&pa2b_pl=&pa2d_dd=&pa2d_mm=&pa2d_yyyy=&pa2d_pl=&pa2_occupation=&e_name1=%23marr&e_place1=&e_date1_dd=&e_date1_mm=&e_date1_yyyy=1944&e_date1_prec=about&e_date1_cal=G&e_date1_orday=&e_date1_ormonth=&e_date1_oryear=&e_date1_text=&e_note1=&e_src1=&e1_witn1_p=link&e1_witn1_kind=&e1_witn1_fn=Bienaim%C3%A9&e1_witn1_occ=0&e1_witn1_sn=Test&e1_ins_witn1_n=1&ins_event1_n=1&ins_ch0_n=1&ch1_p=link&ch1_fn=Henri&ch1_occ=0&ch1_sn=Test&ch1b_dd=&ch1b_mm=&ch1b_yyyy=&ch1b_pl=&ch1d_dd=&ch1d_mm=&ch1d_yyyy=&ch1d_pl=&ch1_occupation=&ins_ch1_n=1&ch2_p=link&ch2_fn=Francis&ch2_occ=0&ch2_sn=Test&ch2b_dd=&ch2b_mm=&ch2b_yyyy=&ch2b_pl=&ch2d_dd=&ch2d_mm=&ch2d_yyyy=&ch2d_pl=&ch2_occupation=&ins_ch2_n=1&ch3_p=&ch3_fn=&ch3_occ=0&ch3_sn=&ch3b_dd=&ch3b_mm=&ch3b_yyyy=&ch3b_pl=&ch3d_dd=&ch3d_mm=&ch3d_yyyy=&ch3d_pl=&ch3_occupation=&ins_ch3_n=1&psrc=&src=&comment="
curl "$SERVER/$BASE-00?digest=&ip=0&i=0&m=MOD_FAM_OK&pa1_fn=Philippe&pa1_occ=0&pa1_sn=Test&pa1_p=link&pa1b_dd=&pa1b_mm=&pa1b_yyyy=&pa1b_pl=&pa1d_dd=&pa1d_mm=&pa1d_yyyy=&pa1d_pl=&pa1_occupation=&pa2_fn=Janine&pa2_occ=0&pa2_sn=Durand&pa2_p=link&pa2b_dd=&pa2b_mm=&pa2b_yyyy=&pa2b_pl=&pa2d_dd=&pa2d_mm=&pa2d_yyyy=&pa2d_pl=&pa2_occupation=&e_name1=%23marr&e_place1=&e_date1_dd=&e_date1_mm=&e_date1_yyyy=1944&e_date1_prec=about&e_date1_cal=G&e_date1_orday=&e_date1_ormonth=&e_date1_oryear=&e_date1_text=&e_note1=&e_src1=&e1_witn1_p=link&e1_witn1_kind=&e1_witn1_fn=Bienaim%C3%A9&e1_witn1_occ=0&e1_witn1_sn=Test&e1_ins_witn1_n=1&ins_event1_n=1&ins_ch0_n=1&ch1_p=link&ch1_fn=Henri&ch1_occ=0&ch1_sn=Test&ch1b_dd=&ch1b_mm=&ch1b_yyyy=&ch1b_pl=&ch1d_dd=&ch1d_mm=&ch1d_yyyy=&ch1d_pl=&ch1_occupation=&ins_ch1_n=1&ch2_p=link&ch2_fn=Francis&ch2_occ=0&ch2_sn=Test&ch2b_dd=&ch2b_mm=&ch2b_yyyy=&ch2b_pl=&ch2d_dd=&ch2d_mm=&ch2d_yyyy=&ch2d_pl=&ch2_occupation=&ins_ch2_n=1&ch3_p=&ch3_fn=&ch3_occ=0&ch3_sn=&ch3b_dd=&ch3b_mm=&ch3b_yyyy=&ch3b_pl=&ch3d_dd=&ch3d_mm=&ch3d_yyyy=&ch3d_pl=&ch3_occupation=&ins_ch3_n=1&psrc=&src=&comment=" > $TMP_DIR/tmp.txt
grep "title>" $TMP_DIR/tmp.txt

$BIN_DIR/gwfixbase $BASES_DIR/$BASE-00
echo ""

$BIN_DIR/gwc -nofail -f $BASES_DIR/$BASE-01.gw -o $BASES_DIR/$BASE-01 > $BASES_DIR/comm.log
read -p "Suite? " suite
echo "******************************************************************"

$BIN_DIR/gwu $BASES_DIR/$BASE-00 -o $BASES_DIR/$BASE-02.gw > $BASES_DIR/comm.log
test_comm $BASES_DIR/comm.log
echo "Gwu $BASE-00.gwb to $BASE-02.gw"

echo ""
diff $BASES_DIR/$BASE-00.gw $BASES_DIR/$BASE-02.gw > $TMP_DIR/tmp.txt
test_diff $TMP_DIR/tmp.txt
echo "diff $BASE-00.gw $BASE-02.gw."
echo "Detailed infos suppressed. Dates stay as 0b, 0p, 0d"

read -p "Suite? " suite
echo "******************************************************************"

$BIN_DIR/gwc -nofail -f $BASES_DIR/$BASE-02.gw -o $BASES_DIR/$BASE-02 > $BASES_DIR/comm.log
test_comm $BASES_DIR/comm.log
echo "Gwc to $BASE-02.gwb"

echo "Gwu $BASE-02.gwb to $BASE-03.gw"
$BIN_DIR/gwu $BASES_DIR/$BASE-02  -o $BASES_DIR/$BASE-03.gw > $BASES_DIR/comm.log
grep "Error:" $BASES_DIR/comm.log
$BIN_DIR/gwc -nofail -f $BASES_DIR/$BASE-03.gw -o $BASES_DIR/$BASE-03 > $BASES_DIR/comm.log
test_comm $BASES_DIR/comm.log
echo "Gwc to $BASE-03.gwb"

$BIN_DIR/gwu $BASES_DIR/$BASE-03  -o $BASES_DIR/$BASE-04.gw > $BASES_DIR/comm.log
test_comm $BASES_DIR/comm.log
echo "Gwu $BASE-03.gwb to $BASE-04.gw"

echo "Gwc to $BASE-04.gwb"
$BIN_DIR/gwc -nofail -f $BASES_DIR/$BASE-04.gw -o $BASES_DIR/$BASE-04 > $BASES_DIR/comm.log
test_comm $BASES_DIR/comm.log
echo "Gwc to $BASE-04.gwb"

echo ""
echo ""
diff $BASES_DIR/$BASE-02.gw $BASES_DIR/$BASE-03.gw > $TMP_DIR/tmp.txt
test_diff $TMP_DIR/tmp.txt
echo "diff $BASE-02.gw $BASE-03.gw. First cycle 02 -> 03"
echo "Should be the same"

read -p "Suite? " suite
echo "******************************************************************"

echo ""
echo ""
diff $BASE-02.gw $BASES_DIR/$BASE-04.gw > $TMP_DIR/tmp.txt
test_diff $TMP_DIR/tmp.txt
echo "diff $BASE-02.gw $BASE-04.gw. One full cycle 02 -> 03 -> 04"
echo "Should be the same"

read -p "Suite? " suite
echo "******************************************************************"

echo "Gwu $BASE-04.gwb to -gwplus $BASE-05.gw"
$BIN_DIR/gwu $BASES_DIR/$BASE-04 -gwplus -o $BASES_DIR/$BASE-05.gw > $BASES_DIR/comm.log
diff $BASES_DIR/$BASE-00.gw $BASES_DIR/$BASE-05.gw > $TMP_DIR/tmp.txt
test_diff $TMP_DIR/tmp.txt
echo "diff $BASE-00.gw $BASE-05.gw."
echo "Final test: compare initial .gw with 05.gw"

rm $TMP_DIR/tmp.txt
rm $BASES_DIR/comm.log

echo ""
echo "Done"
