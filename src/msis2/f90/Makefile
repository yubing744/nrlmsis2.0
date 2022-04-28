.PHONY : build-test
build-test:
	gfortran -O3 -cpp -o msis2.0_test alt2gph.F90 msis_constants.F90 msis_init.F90 msis_gfn.F90 msis_tfn.F90 msis_dfn.F90 msis_calc.F90 msis_gtd8d.F90 msis2.0_test.F90

.PHONY : run-test
run-test: build-test
	./msis2.0_test

.PHONY : build-export-parm
build-export-parm:
	gfortran -O3 -cpp -o msis2.0_export_parm alt2gph.F90 msis_constants.F90 msis_init.F90 msis_gfn.F90 msis_tfn.F90 msis_dfn.F90 msis_calc.F90 msis_gtd8d.F90 msis2.0_export_parm.F90

.PHONY : run-export-parm
run-export-parm: build-export-parm
	./msis2.0_export_parm