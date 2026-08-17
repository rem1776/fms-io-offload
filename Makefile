all: writer.x model.x

FMS_BUILD= /home/Ryan.Mulhall/fms-io-offload/fms/build
FCFLAGS=-g -traceback -O0

model.x: model.F90
	mpiifx ${FCFLAGS} -o model.x -lFMS -L${FMS_BUILD}/libFMS/.libs -I${FMS_BUILD}/.mods model.F90

writer.x: writer.F90
	mpiifx ${FCFLAGS} -o writer.x -lFMS -L${FMS_BUILD}/libFMS/.libs -I${FMS_BUILD}/.mods writer.F90

clean:
	rm *.x
