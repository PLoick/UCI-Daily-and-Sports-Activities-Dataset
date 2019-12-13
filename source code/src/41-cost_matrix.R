cost_matrix <-
data.table::fread("
Sport  a01 a02 a03 a04 a05 a06 a07 a08 a09 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19
a01      0   1   1   1   2   2   2   2   2   2   2   3   3   3   3   3   3   3   3
a02      1   0   1   1   2   2   2   2   2   2   2   3   3   3   3   3   3   3   3
a03      1   1   0   1   2   2   2   2   2   2   2   3   3   3   3   3   3   3   3
a04      1   1   1   0   2   2   2   2   2   2   2   3   3   3   3   3   3   3   3
a05      2   2   2   2   0   1   1   1   1   1   1   2   2   2   2   2   2   2   2
a06      2   2   2   2   1   0   1   1   1   1   1   2   2   2   2   2   2   2   2
a07      2   2   2   2   1   1   0   1   1   1   1   2   2   2   2   2   2   2   2
a08      2   2   2   2   1   1   1   0   1   1   1   2   2   2   2   2   2   2   2
a09      2   2   2   2   1   1   1   1   0   1   1   2   2   2   2   2   2   2   2
a10      2   2   2   2   1   1   1   1   1   0   1   2   2   2   2   2   2   2   2
a11      2   2   2   2   1   1   1   1   1   1   0   2   2   2   2   2   2   2   2
a12      3   3   3   3   2   2   2   2   2   2   2   0   1   1   1   1   1   1   1
a13      3   3   3   3   2   2   2   2   2   2   2   1   0   1   1   1   1   1   1
a14      3   3   3   3   2   2   2   2   2   2   2   1   1   0   1   1   1   1   1
a15      3   3   3   3   2   2   2   2   2   2   2   1   1   1   0   1   1   1   1
a16      3   3   3   3   2   2   2   2   2   2   2   1   1   1   1   0   1   1   1
a17      3   3   3   3   2   2   2   2   2   2   2   1   1   1   1   1   0   1   1
a18      3   3   3   3   2   2   2   2   2   2   2   1   1   1   1   1   1   0   1
a19      3   3   3   3   2   2   2   2   2   2   2   1   1   1   1   1   1   1   0
")

# Diagonal = 0
# Similar Activities = 1
# Dissimilar Activities = 2-3
# 
# Activity                         Abbreviation
#    <chr>                            <chr>       
#   1 sitting                          a01         
#   2 standing                         a02         
#   3 lying on back                    a03         
#   4 lying on right side              a04         
#   5 ascending stairs                 a05         
#   6 descending stairs                a06         
#   7 still in an elevator             a07         
#   8 moving in an elevator            a08         
#   9 walking in a parking lot         a09         
#  10 walking on a flat treadmill      a10         
#  11 walking on an inclined treadmill a11         
#  12 running on a treadmill           a12         
#  13 exercising on a stepper          a13         
#  14 exercising on a cross trainer    a14         
#  15 cycling horizontally             a15         
#  16 cycling vertically               a16         
#  17 rowing                           a17         
#  18 jumping                          a18         
#  19 playing basketball               a19