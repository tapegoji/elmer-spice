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

dimensions = interface.get_mesh_dimensions(meshName)

N = 1
# Define mesh coordinates and register coordinates
grid = np.zeros([N , dimensions])
grid[:, 0] = np.linspace(0, N-1, N )  # x component
grid[:, 1] = 0  # np.linspace(0, config.L, N+1)  # y component, leave blank

Vin = 650
R1 = 0.1

trace_resistance = 1e-3 # initial guess

pad_currents = [-Vin / (R1 + trace_resistance)]
pad_areas = [2.927599612256225e-6]  # for now, we have only one pad
pad_current_density = np.array(pad_currents)/np.array(pad_areas)

writeData = pad_current_density * np.ones(N)

vertexIDs = interface.set_mesh_vertices(meshName, grid)
print('------------------vertexIDs:', vertexIDs)

if interface.requires_initial_data():
    print("preCICE: requires initial data ...")
    interface.write_data(meshName, writeDataName,
                         vertexIDs, writeData)
    
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


    print('---------------------------------------------reading potentials from preCICE----------------------------')
    pad_potentials = interface.read_data(
        meshName, readDataName, vertexIDs, precice_dt)

    print(pad_potentials)
    
    # calculate the trace resistance
    print('---------------------------------------------trace resistance calculation------------------------------------')
    print('pad_currents:', pad_currents)
    trace_resistance = (Vin - pad_potentials[0]) / math.fabs(pad_currents[0])
    print(trace_resistance)
    
    # dummy spice simulation
    # set the current out of the pad to be negative
    pad_currents = [-Vin/(R1 + trace_resistance)]
    pad_current_density = np.array(pad_currents)/np.array(pad_areas)

    print('---------------------------------------------writing currents densities to preCICE----------------------------')
    writeData = pad_current_density * np.ones(N)
    print(writeData)
    interface.write_data(meshName, writeDataName,
                         vertexIDs, writeData)

    # Advance the coupling
    interface.advance(precice_dt)

    if interface.requires_reading_checkpoint():
        pass


print("Exiting Spice Solver")
