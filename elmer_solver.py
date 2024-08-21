import numpy as np
import precice

print("Starting Elmer Solver...")

print("Configure preCICE...")
interface = precice.Participant("Elmer", "precice-config.xml", 0, 1)
print("preCICE configured...")

meshName = "Elmer-Nodes-Mesh"
readDataName = "Current Density"
writeDataName = "Potential"

dimensions = interface.get_mesh_dimensions(meshName)

N = 5
L = 5
# Define mesh coordinates and register coordinates
grid = np.zeros([N + 1, dimensions])
grid[:, 0] = np.linspace(0, L, N + 1)  # x component
grid[:, 1] = 0  # np.linspace(0, config.L, N+1)  # y component, leave blank

pad_potentials = [650]
writeData = pad_potentials[0] * np.ones(N + 1)
vertexIDs = interface.set_mesh_vertices(meshName, grid)

if interface.requires_initial_data():
    interface.write_data(meshName, writeDataName,
                         vertexIDs, writeData)
    
print("Elmer: init precice...")

interface.initialize()

pad_current_densities = interface.read_data(meshName, readDataName, vertexIDs, 0)

while interface.is_coupling_ongoing():
    # When an implicit coupling scheme is used, checkpointing is required
    if interface.requires_writing_checkpoint():
        pass
    precice_dt = interface.get_max_time_step_size()

    # Read data, solve timestep, and write data
    pad_current_densities = interface.read_data(
        meshName, readDataName, vertexIDs, precice_dt)
    interface.write_data(meshName, writeDataName,
                         vertexIDs, writeData)
    
    # Advance the coupling
    interface.advance(precice_dt)

    if interface.requires_reading_checkpoint():
        pass    


print("Exiting Elmer Solver")
