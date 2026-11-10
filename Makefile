default: offload_writer_driver.x offload_model_driver.x baseline_test_driver.x

LDFLAGS=-Lfms/build/libFMS/.libs -lFMS
INC_FLAGS=-Ifms/build/.mods

CC=cc
FC=ftn

offload_writer_driver.x: offload_writer_driver.F90
	${FC} ${LDFLAGS} ${INC_FLAGS} ${FCFLAGS} -g -traceback offload_writer_driver.F90 -o offload_writer_driver.x

offload_model_driver.x: offload_model_driver.F90
	${FC} ${LDFLAGS} ${INC_FLAGS} ${FCFLAGS} -g -traceback offload_model_driver.F90 -o offload_model_driver.x

baseline_test_driver.x: baseline_test_driver.F90
	${FC} ${LDFLAGS} ${INC_FLAGS} ${FCFLAGS} -g -traceback baseline_test_driver.F90 -o baseline_test_driver.x

clean:
	rm *.x 
