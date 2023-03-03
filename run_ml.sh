#!/bin/bash
#
#$ -cwd -V -b y
#$ -P vlong
#$ -l h_vmem=400G -l fourperhost=4
#$ -o tmp.out -e tmp.err
#

Rscript benchmark.R config_post2018.csv 1 FALSE TRUE 1 $SGE_TASK_ID.Rout