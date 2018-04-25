
#move the directory
#../data/friends_info/edgelist_Feb27/timelines_csv

#merge the small files under each folder --> 56 files
for folder in t*
do

cd $folder
for file in *
do
sed '1d' $file > ../out/$file
echo $file
done
cd ..

cat header.csv out/* > f$folder.csv
rm -rf out/*
rm -rf $folder
echo $folder 
done


# part1 share the timelines were collected around the similar time
# ft01, 02, share the time that followers start to follow Trump


#rename all the files in the current dir: filename --> Unix_filename
#1. rename 's/^/Unix_/' *
#2. ls | xargs -i mv {} unix_{}


#remove prefix of filenames:  before '_'
#rename "s/.*_//" *



touch TEN_GOP.txt
touch Crystal1Johnson.txt
touch Pamela_Moore13.txt
touch gloed_up.txt
touch Jenn_Abrams.txt
touch TrayneshaCole.txt
for file in ft*
do  
egrep 'TEN_GOP|MemphisBlacks|4224729994' $file >> TEN_GOP.txt
egrep 'Crystal1Johnson|4437233895' $file >> Crystal1Johnson.txt
egrep 'Pamela_Moore13|SouthUnitedUs|4272870988' $file >> Pamela_Moore13.txt
egrep 'gloed_up|4mysquad|3312143142' $file >> gloed_up.txt
egrep 'Jenn_Abrams|JennaTraveller|2882331822' $file >> Jenn_Abrams.txt
egrep 'TrayneshaCole|4859142199' $file >> TrayneshaCole.txt
done






