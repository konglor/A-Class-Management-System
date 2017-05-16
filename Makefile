# A Class Management System
# Kongmeng Lor
# Instructor: Ahmed Naumaan
# College: Saint Paul College
# CSCI 2469-01 Advance Programming Principles
# 05.05.2017

COMPILER = ocamlc

LIB = unix.cma \
	str.cma

MODULE = oper.ml \
	util.ml \
	role.ml \
	course.ml \
	serialize.ml \
	college.ml \
	cmdfunc.ml \
	cmd.ml \
	state.ml \
	main.ml

build: $(MODULE)
	$(COMPILER) -o classmgr $(LIB) $(MODULE)

clean:
	rm -f *.cmi
	rm -f *.cmo
	rm classmgr