# clean notebook's outputs before adding
git config --local filter.clean-notebook-output.clean "jupyter nbconvert --clear-output --inplace --stdin --stdout"