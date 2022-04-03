notebook=$1

jupyter nbconvert --execute --allow-errors --to markdown $notebook
jupyter nbconvert --clear-output --inplace $notebook