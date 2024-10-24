import numpy as np
import precice
import math

print("Starting Spice Solver...")

print("Configure preCICE...")
interface = precice.Participant("Spice", "precice-config.xml", 0, 1)
print("preCICE configured...")

meshName = "Spice-Nodes-Mesh"
readDataName = "Potential"
writeDataName = "Current Density"
writeDataName2 = "NewPotential"

dimensions = interface.get_mesh_dimensions(meshName)

N = 1
commsize = 2
# Define mesh coordinates and register coordinates
grid = np.zeros([commsize , dimensions])
for i in range(N):
    for j in range(commsize):
        grid[i] = [i, j]

Vin = 650
R1 = 1e3

trace_resistance = 1e-3 # initial guess

pad_currents = [-Vin / (R1 + trace_resistance)]
pad_areas = [2.927599612256225e-6]  # for now, we have only one pad
pad_current_density = np.array(pad_currents)/np.array(pad_areas)

writeData = pad_current_density * np.ones([commsize, N])
writeData2 = Vin * np.ones([commsize, N])

vertexIDs = interface.set_mesh_vertices(meshName, grid)
print('------------------vertexIDs:', vertexIDs)

if interface.requires_initial_data():
    print("preCICE: requires initial data ...")
    interface.write_data(meshName, writeDataName,  vertexIDs, writeData)
    interface.write_data(meshName, writeDataName2, vertexIDs, writeData2)
    
print("Spice: init precice...")

interface.initialize()

pad_potentials = interface.read_data(meshName, readDataName, vertexIDs, 0)
iter = 0
while interface.is_coupling_ongoing():
    iter += 1
    # When an implicit coupling scheme is used, checkpointing is required
    if interface.requires_writing_checkpoint():
        pass
    precice_dt = interface.get_max_time_step_size()

    # prin the current iteration
    print('---------------------------------------------iteration:', iter, '------------------------------------')
    # print('---------------------------------------------reading potentials from preCICE----------------------------')
    pad_potentials = interface.read_data(
        meshName, readDataName, vertexIDs, precice_dt)

    print('potentials:', pad_potentials)
    
    # calculate the trace resistance
    # print('---------------------------------------------trace resistance calculation------------------------------------')
    print('pad_currents:', pad_currents)
    if pad_currents[0] == 0:
        trace_resistance = 1e-3
    else:
        trace_resistance = (Vin - pad_potentials[0]) / math.fabs(pad_currents[0])
    print('trace_resistance:', trace_resistance )
    
    
    # dummy spice simulation
    # set the current out of the pad to be negative
    pad_currents = [-Vin/(R1 + trace_resistance)]
    pad_current_density = np.array(pad_currents)/np.array(pad_areas)

    # print('---------------------------------------------writing currents densities to preCICE----------------------------')
    writeData = pad_current_density * np.ones([commsize, N])
    print('writeData:', writeData)
    interface.write_data(meshName, writeDataName,  vertexIDs, writeData)
    interface.write_data(meshName, writeDataName2, vertexIDs, writeData2)

    # Advance the coupling
    interface.advance(precice_dt)

    if interface.requires_reading_checkpoint():
        pass


print("Exiting Spice Solver")
