.PHONY : build
build:
	gfortran -O3 -cpp -o msis2.0_test alt2gph.F90 msis_constants.F90 msis_init.F90 msis_gfn.F90 msis_tfn.F90 msis_dfn.F90 msis_calc.F90 msis_gtd8d.F90 msis2.0_test.F90

.PHONY : build-export
build-export:
	gfortran -O3 -cpp -o msis2.0_export alt2gph.F90 msis_constants.F90 msis_init.F90 msis_param_export.F90 msis_gfn.F90 msis_tfn.F90 msis_dfn.F90 msis_calc.F90 msis_gtd8d.F90 msis2.0_export.F90

.PHONY : run-export
run-export: build-export
	./msis2.0_export