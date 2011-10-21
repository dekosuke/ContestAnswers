one2nineL = l "onetwothreefourfivesixseveneightnine"
hundred = "hundred"
hl = length hundred

l=length
lettersBeforeHundred = one2nineL + l "ten" + l "eleven" + l "twelve" +
 l "thirteen" + l "fourteen" + l "fifteen" + l "sixteen" + l "seventeen" +
 l "eighteen" + l "nineteen" + 
 (l "twenty" + l "thirty" + l "forty" + l "fifty" + l "sixty" + l "seventy" +
  l "eighty" + l "ninety") * 10 + one2nineL * 8
letters = lettersBeforeHundred * 10 + hl*900 + (l "and") * 99 * 9+ one2nineL*100 + l "onethousand"
main = print letters
