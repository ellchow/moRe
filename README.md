moRe
========

a collection of R functions and utilities. 

*utils*

utilities for improving day-to-day data exploration

*mdls*

tools for modeling, focusing mainly on gbm and (g)lm

*cmdargs*

simple command-line argument parser

*math*

math/stats related like sampling, bucketization, bootstrapping, etc.

*infor*

handy for information retrieval

*plots*

helpers for plotting (experimental)

*sql*

some wrappers for accessing databases and using SQL

*import*

since I am too lazy to make a real R library, I add source this file (with chdir=T) in my .Rprofile so that i can call my custom import function to load my pseudo-modules as well as normal libraries (auto-installs if missing).

for simple "installation", _run setup.sh_.

*yahoofin*

functions for accessing yahoo finance data

*hadoopr*

hadoop streaming helpers

*standalone*

scripts for building a standalone instance of R - only tried on ubuntu

*infor-vis*

shiny app for visualizing information retrieval results
