moRe
========

really is just a bunch of functions to make R nicer to use.  a large part is dedicated to modeling as that is what I have been working on lately.

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

since I am too lazy to make a real R library (and it seems a bit too heavy duty for me), I add source this file (with chdir=T) in my .Rprofile so that i can call my custom import function to load my pseudo-modules as well as normal libraries (auto-installs if missing).

for simple "installation", _run setup.sh_.

*yahoofin*

functions for accessing yahoo finance data



