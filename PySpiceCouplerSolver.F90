MODULE HelperMethods

    !------------------------------------------------------------------------------
    USE DefUtils
    !------------------------------------------------------------------------------
    IMPLICIT NONE

    

    CONTAINS

    SUBROUTINE Print(dataName,mesh,BoundaryPerm,CoordVals)

        !-------------------------Strings----------------------------------------------
        CHARACTER(LEN=MAX_NAME_LEN)         :: dataName
        CHARACTER(LEN=MAX_NAME_LEN)         :: infoMessage
        !-------------------------Elmer_Types----------------------------------------------
        TYPE(Variable_t), POINTER           :: dataVariable
        TYPE(Mesh_t), POINTER               :: mesh
        !------------------------Data Arrays----------------------------------------------
        REAL(KIND=dp), POINTER              :: CoordVals(:)
        INTEGER, POINTER                    :: BoundaryPerm(:)
        !--------------------------Iterators-------------------------------------
        INTEGER                             :: i,j
        !--------------------------Mesh-------------------------------------
        INTEGER                         :: meshDim
        dataVariable  => VariableGet( mesh % Variables, dataName)
        meshDim = mesh % MaxDim
        CALL Info('PySpiceCouplerSolver','Printing ' //TRIM(dataName))
        DO i = 1, mesh % NumberOfNodes
            j = BoundaryPerm(i)
            IF(j == 0) CYCLE
            IF (meshDim == 2) THEN
                write(infoMessage,'(A,I5,A,I5,A,F10.6,A,F10.6,A,F10.6)') 'Node: ',i,' Index: ',j,' Value: ' &
                            ,dataVariable % Values(dataVariable % Perm(i)),&
                            ' X= ', CoordVals(meshDim * j-1), ' Y= ', CoordVals(meshDim * j) 
            ELSE IF (meshDim == 3) THEN
                write(infoMessage,'(A,I5,A,I5,A,F10.6,A,F10.6,A,F10.6,A,F10.6)') 'Node: ',i,' Index: ',j,' Value: ' &
                            ,dataVariable % Values(dataVariable % Perm(i)),&
                            ' X= ', CoordVals(meshDim * j-2), ' Y= ', CoordVals(meshDim *j-1), ' Z= ', CoordVals(meshDim *j) 
            END IF                                        
            CALL Info('PySpiceCouplerSolver',infoMessage)

        END DO 

    END SUBROUTINE Print

    SUBROUTINE PrintDomain(dataName,mesh)

        !-------------------------Strings----------------------------------------------
        ! CHARACTER(LEN=MAX_NAME_LEN)         :: dataName
        CHARACTER(LEN=MAX_NAME_LEN)         :: infoMessage
        character(len=*), intent(in) ::      dataName
        !-------------------------Elmer_Types----------------------------------------------
        TYPE(Variable_t), POINTER           :: dataVariable
        TYPE(Mesh_t), POINTER               :: mesh
        !------------------------Data Arrays----------------------------------------------
        REAL(KIND=dp), POINTER              :: CoordVals(:)
        INTEGER, POINTER                    :: BoundaryPerm(:)
        !--------------------------Iterators-------------------------------------
        INTEGER                             :: i,j
    
        dataVariable  => VariableGet( mesh % Variables, dataName)

        CALL Info('PySpiceCouplerSolver','Printing ' //TRIM(dataName))
        DO i = 1, mesh % NumberOfNodes
            
            
            write(infoMessage,'(A,I5,A,F10.4,A,F10.6,A,F10.6)') 'Node: ',i,' Value: ' &
                            ,dataVariable % Values(dataVariable % Perm(i)),&
                            ' X= ', mesh % Nodes % x(i), ' Y= ', mesh % Nodes % y(i) 
                                                          
            CALL Info('PySpiceCouplerSolver',infoMessage)

        END DO 
    END SUBROUTINE PrintDomain

    SUBROUTINE CreateVariable(dataName,dataType,mesh,BoundaryPerm,Solver,solverParams)
        !-------------------------Strings-----------------------------------------------
        CHARACTER(LEN=MAX_NAME_LEN)         :: dataName
        CHARACTER(LEN=MAX_NAME_LEN)         :: infoMessage
        character(len=*), intent(in) :: dataType
        !-------------------------Elmer_Types-------------------------------------------
        TYPE(Variable_t), POINTER           :: dataVariable
        TYPE(Mesh_t), POINTER               :: mesh
        TYPE(Solver_t)                      :: Solver
        TYPE(ValueList_t), POINTER          :: solverParams
        !------------------------Data Arrays--------------------------------------------
        INTEGER, POINTER                    :: BoundaryPerm(:)
        !------------------------Mesh Data----------------------------------------------
        INTEGER                         :: Dofs
        !--------------------------Logic-Control-------------------------------------
        LOGICAL                             :: Found

        
        dataName =  ListGetString(solverParams,dataType,Found )
        dataVariable  => VariableGet( mesh % Variables, dataName)
        IF(ASSOCIATED( dataVariable ) ) THEN
            CALL Info('PySpiceCouplerSolver','Using existing variable : '//TRIM(dataName) )
        ELSE
            CALL FATAL('PySpiceCouplerSolver', 'Variable does not exist : ' // TRIM(dataName) )
            ! Dofs = ListGetInteger( solverParams,'Field Dofs',Found )
            ! IF(.NOT. Found ) Dofs = 1
            ! CALL VariableAddVector( mesh % Variables, mesh, Solver, dataName, Dofs, &
            !     Perm = BoundaryPerm, Secondary = .TRUE. )
            ! dataVariable => VariableGet( mesh % Variables, dataName ) 
        END IF


    END SUBROUTINE CreateVariable
    
    

    SUBROUTINE CopyReadData(dataName,mesh,BoundaryPerm,copyData)
        !-------------------------Strings-----------------------------------------------
        CHARACTER(LEN=MAX_NAME_LEN)         :: dataName
        !-------------------------Elmer_Types----------------------------------------------
        TYPE(Variable_t), POINTER           :: dataVariable
        TYPE(Mesh_t), POINTER               :: mesh
        ! !------------------------Data Arrays----------------------------------------------
        REAL(KIND=dp), POINTER              :: copyData(:)
        INTEGER, POINTER                    :: BoundaryPerm(:)
        !--------------------------Iterators-------------------------------------
        INTEGER                             :: i,j


        dataVariable  => VariableGet( mesh % Variables, dataName)

        DO i = 1, mesh % NumberOfNodes
            j = BoundaryPerm(i)
            IF(j == 0) CYCLE
            dataVariable % Values(dataVariable % Perm(i)) = copyData(j)
            

        END DO 

    END SUBROUTINE CopyReadData

    SUBROUTINE CopyWriteData(dataName,mesh,BoundaryPerm,copyData)
        !-------------------------Strings-----------------------------------------------
        CHARACTER(LEN=MAX_NAME_LEN)         :: dataName
        !-------------------------Elmer_Types----------------------------------------------
        TYPE(Variable_t), POINTER           :: dataVariable
        TYPE(Mesh_t), POINTER               :: mesh
        !------------------------Data Arrays----------------------------------------------
        REAL(KIND=dp), POINTER              :: copyData(:)
        INTEGER, POINTER                    :: BoundaryPerm(:)
        !--------------------------Iterators-------------------------------------
        INTEGER                             :: i,j


        dataVariable  => VariableGet( mesh % Variables, dataName)

        DO i = 1, mesh % NumberOfNodes
            j = BoundaryPerm(i)
            IF(j == 0) CYCLE
            copyData(j) = dataVariable % Values(dataVariable % Perm(i))
            ! IF( dataName == "temperature loads") THEN
            !     copyData(j) = -1 * dataVariable % Values(dataVariable % Perm(i)) 
            ! ELSE
            !     copyData(j) = dataVariable % Values(dataVariable % Perm(i))
            ! END IF
        END DO 
    END SUBROUTINE CopyWriteData

    SUBROUTINE GetMaxData(dataName,mesh,BoundaryPerm,dmax)
        !-------------------------Strings-----------------------------------------------
        CHARACTER(LEN=MAX_NAME_LEN)         :: dataName
        !-------------------------Elmer_Types----------------------------------------------
        TYPE(Variable_t), POINTER           :: dataVariable
        TYPE(Mesh_t), POINTER               :: mesh
        !------------------------Data Arrays----------------------------------------------
        INTEGER, POINTER                    :: BoundaryPerm(:)
        !--------------------------Iterators-------------------------------------
        INTEGER                             :: i,j
        !--------------------------data-------------------------------------
        REAL(KIND=dp)                       :: dmax


        dataVariable  => VariableGet( mesh % Variables, dataName)
        dmax = -HUGE(dmax)
        DO i = 1, mesh % NumberOfNodes
            j = BoundaryPerm(i)
            IF(j == 0) CYCLE
                dmax = MAX(dmax, dataVariable % Values(dataVariable % Perm(i)))
            ! copyData(j) = dataVariable % Values(dataVariable % Perm(i))
            ! IF( dataName == "temperature loads") THEN
            !     copyData(j) = -1 * dataVariable % Values(dataVariable % Perm(i)) 
            ! ELSE
            !     copyData(j) = dataVariable % Values(dataVariable % Perm(i))
            ! END IF
        END DO
    END SUBROUTINE GetMaxData

END MODULE HelperMethods



SUBROUTINE PySpiceCouplerSolver( Model,Solver,dt,TransientSimulation)

    !------------------------------------------------------------------------------
    USE DefUtils
    USE HelperMethods
    !------------------------------------------------------------------------------
    IMPLICIT NONE
    !--------------------------UDS Prerequistes------------------------------------
    TYPE(Solver_t) :: Solver
    TYPE(Model_t) :: Model
    REAL(KIND=dp) :: dt
    LOGICAL :: TransientSimulation


    !--------------------------Variables-Start-------------------------------------------
    !--------------------------Logic-Control-------------------------------------
    LOGICAL                             :: Found
    !--------------------------MPI-Variables-------------------------------------
    INTEGER                         :: rank,commsize
   
    !--------------------------Elmer-Variables-------------------------------------
    !-------------------------Loop_Control-------------------------------------
    INTEGER                         :: itask = 1
    !-------------------------Strings----------------------------------------------
    CHARACTER(LEN=MAX_NAME_LEN)         :: BoundaryName
    CHARACTER(LEN=MAX_NAME_LEN)         :: infoMessage
    !-------------------------Elmer_Types----------------------------------------------
    TYPE(Variable_t), POINTER           :: readDataVariable,writeDataVariable
    TYPE(Mesh_t), POINTER               :: mesh
    TYPE(ValueList_t), POINTER          :: simulation, solverParams, BC ! Simulation gets Simulation list, & solverParams hold solver1,solver 2,etc
    !------------------------Data Arrays----------------------------------------------
    REAL(KIND=dp), POINTER              :: CoordVals(:), nodeCoordVals(:)
    INTEGER, POINTER                    :: BoundaryPerm(:)
    !------------------------Time Variable----------------------------------------------
    TYPE(Variable_t), POINTER :: TimeVar
    Real(KIND=dp) :: Time

    !--------------------------preCICE-Variables-------------------------------------
    !-------------------------Strings----------------------------------------------
    CHARACTER(LEN=MAX_NAME_LEN)         :: config
    CHARACTER(LEN=MAX_NAME_LEN)         :: participantName, meshName
    CHARACTER(LEN=MAX_NAME_LEN)         :: readDataName, writeDataName

    !-------------------------IDs-Integer----------------------------------------------
    INTEGER                         :: meshID,readDataID, writeDataID, meshDim
    !------------------------Data Arrays----------------------------------------------
    REAL(KIND=dp), POINTER :: vertices(:), writeData(:), readData(:)
    INTEGER, POINTER                :: vertexIDs(:), nodeIDs(:)
    REAL(KIND=dp) :: dmax
    !------------------------Mesh Data----------------------------------------------
    INTEGER                         :: BoundaryNodes, numberOfNodes
    INTEGER                         :: Dofs
    INTEGER                         :: dimensions ! ?? Do not know 
    !----------------------Time Loop Control Variables-----------------------------
    INTEGER                         :: bool
    INTEGER                         :: ongoing
    INTEGER                         :: i,j, k
    !--------------------------Variables-End-------------------------------------------

    !--------------------------SAVE-Start-------------------------------------------
    SAVE meshName,readDataName,writeDataName
    SAVE itask
    SAVE BoundaryPerm,CoordVals,vertexIDs, nodeIDs, nodeCoordVals
    SAVE readData,writeData
    SAVE BoundaryNodes, numberOfNodes
    SAVE dmax
    !--------------------------SAVE-End-------------------------------------------


    !--------------------------Initialize-Start-------------------------------------------
    Simulation => GetSimulation()
    Mesh => Solver % Mesh
    solverParams => GetSolverParams()
    meshDim = Mesh % MaxDim
    
    rank = 0
    commsize = 1
    select case(itask)
    case(1)
        CALL Info('PySpiceCouplerSolver ', 'Initializing Coupler Solver')
        !--- First Time Visited, Initialization
        !-- First Time Visit, Create preCICE, create nodes at interface
        !----------------------------- Initialize---------------------
        
        !----------------Acquire Names for solver---------------------
        ! BoundaryName = GetString( Simulation, 'maskName', Found )
        BoundaryName = 'R1_1'
        participantName = GetString( Simulation, 'participantName', Found )
        
        !-----------------Convert to preCICE Naming Convention    
        IF (participantName == 'elmer') THEN
            participantName = 'Elmer'
        END IF

        meshName = GetString( Simulation, 'meshName', Found )
        IF (meshName == 'elmer-nodes-mesh') THEN
            meshName = 'Elmer-Nodes-Mesh'
        END IF
        !---------------------------------------------------------------------

        !-----------------Get Config Path-------------------------------------
        config = GetString( Simulation, 'configPath', Found )

        Print *, TRIM(participantName)," ",TRIM(meshName)," ",TRIM(config)
        
        !-----------Identify Vertex on Coupling Interface & Save Coordinates--------------------
        NULLIFY( BoundaryPerm )    
        ALLOCATE( BoundaryPerm( Mesh % NumberOfNodes ) )
        BoundaryPerm = 0
        BoundaryNodes = 0
        CALL MakePermUsingMask( Model,Model%Solver,Model%Mesh,BoundaryName,.FALSE., &
            BoundaryPerm,BoundaryNodes)
            
        CALL Info('PySpiceCouplerSolver','Number of nodes at interface:'//TRIM(I2S(BoundaryNodes)))
        ALLOCATE( CoordVals(meshDim * BoundaryNodes) )
        ALLOCATE(vertexIDs(BoundaryNodes)) 
        DO i=1,Mesh % NumberOfNodes
            j = BoundaryPerm(i)
            IF(j == 0) CYCLE
            IF (meshDim == 2) THEN
                CoordVals(meshDim *j-1) = mesh % Nodes % x(i)
                CoordVals(meshDim*j)    = mesh % Nodes % y(i)
            ELSE IF (meshDim == 3) THEN
                CoordVals(meshDim *j-2) = mesh % Nodes % x(i)
                CoordVals(meshDim *j-1) = mesh % Nodes % y(i)
                CoordVals(meshDim *j)   = mesh % Nodes % z(i)                
            END IF            
        END DO
        ! ALLOCATE(writeData(BoundaryNodes*meshDim))
        
 
        CALL Info('PySpiceCouplerSolver','Created nodes at interface')  

        ! !-----------Identify read and write Variables and Create it if it does not exist--------------------
        ! CALL CreateVariable(readDataName,'readDataName',mesh,BoundaryPerm,Solver,solverParams)
        CALL CreateVariable(writeDataName,'writeDataName',mesh,BoundaryPerm,Solver,solverParams)
        !-----------------------------------------------------------------------------------------

        ! !---------------Initializing preCICE------------------------------------------ 
        CALL Info('CouplerSolver','Initializing preCICE')     
        CALL precicef_create(participantName, config, rank, commsize)
        
        CALL Info('CouplerSolver','Setting up mesh in preCICE')
        CALL precicef_get_mesh_dimensions(meshName, dimensions)
        
        
        ! TODO: This is a placeholder for now. This will be replaced by the actual number of nodes
        numberOfNodes = 1
        
        
        ALLOCATE( nodeCoordVals( numberOfNodes * dimensions ) )
        ALLOCATE( nodeIDs( numberOfNodes ) )
        Do i = 1, numberOfNodes
            nodeCoordVals(dimensions * i - 1) = i
            nodeCoordVals(dimensions * i) = 0.0
        END DO
        print *, 'number of nodes: ', numberOfNodes
        print *, 'coordinates: ', nodeCoordVals
        CALL precicef_set_vertices(meshName, numberOfNodes, nodeCoordVals, nodeIDs)
        print *, 'nodeIDs: ', nodeIDs

        ALLOCATE(readData(numberOfNodes))
        ALLOCATE(writeData(numberOfNodes *dimensions))

        readData = 0
        writeData = 0
        
        CALL precicef_requires_initial_data(bool)
        IF (bool.EQ.1) THEN
            CALL Info('CouplerSolver', 'preCICE requires initial data')
            CALL precicef_write_data(meshName, 'Potential', numberOfNodes, nodeIDs, writeData)
        END IF
        CALL precicef_initialize()
        CALL precicef_is_coupling_ongoing(ongoing)
        
        itask = 2
    case(2)
        ! Place holder for readdata. i.e setting the current density in elmer
        !-----------------------------------------------------------------------------------------
        ! Go through all the BCs and update the current density

        !-------------------Copy Read values from Variable to buffer---------------------
        CALL precicef_requires_reading_checkpoint(bool)

        CALL Info('CouplerSolver ', 'Readinging the data from preCICE', LEVEL=5)                
        CALL precicef_get_max_time_step_size(dt)
        CALL precicef_read_data(meshName, 'Current Density', numberOfNodes, nodeIDs, dt, readData)
        
        ! Convert to CALL Info with LEVEL= 5
        print *, 'Current density that was read from preCICE is: ', readData(1) ! this works, gives the value for the whole boundary

        ! readData contains the current density that was sent by the precice. This will be written to the boundary condition in Elmer
        DO i=1,Model % NumberOfBCs
            BC => Model % BCs(i) % Values
            ! chck if the BC is current density BC
            IF(.NOT. ListGetLogical(BC, &
            'Current Density BC',Found)) CYCLE ! this works too
            ! for R1 to R5
            DO j = 1, 1
                ! for pad 1 and pad 2
                Do k = 1, 1
                    BoundaryName = 'r' // TRIM(I2S(j))// '_'//TRIM(I2S(k))
                    ! check if the BC is the one we are looking for                
                    IF (ListGetString(BC, 'Name', Found) /= BoundaryName) CYCLE
                    CALL Info('PySpiceCouplerSolver ','Updating Current Density for Boundary: '//TRIM(BoundaryName), LEVEL=4)                
                    CALL ListAddConstReal(BC, 'Current Density', REAL(readData(1), dp))
                END DO
            END DO
        END DO
        itask = 3
    case(3)

        !-------------------Copy Write values from Variable to buffer----------------------------
        CALL Info('PySpiceCouplerSolver','Writing data to circuit simulator')

        DO i = 1, 1
            BoundaryName = 'R' // TRIM(I2S(i))// '_1'
            NULLIFY( BoundaryPerm )    
            ALLOCATE( BoundaryPerm( Mesh % NumberOfNodes ) )
            BoundaryPerm = 0
            BoundaryNodes = 0
            CALL MakePermUsingMask( Model,Model%Solver,Model%Mesh,BoundaryName,.FALSE., &
                BoundaryPerm,BoundaryNodes)
            CALL GetMaxData(writeDataName,mesh,BoundaryPerm,dmax)
            writeData = dmax
            CALL precicef_write_data(meshName, 'Potential', numberOfNodes, nodeIDs, writeData)
            ! TODO: Write it using call info
            print *, 'Maximum ', TRIM(writeDataName), ' at ', TRIM(BoundaryName), ' = ', writeData   
            ! CALL Info('PySpiceCouplerSolver: potent', TRIM(writeDataName) , LEVEL=4)
        END DO
        !-------------------Advance preCICE-------------------------------------------------------
        CALL precicef_advance(dt)
        CALL precicef_is_coupling_ongoing(ongoing)

        
        IF(ongoing.EQ.0) THEN
            itask = 4
        ELSE
            itask = 2
        END IF
    end select

    CALL Info('PySpiceCouplerSolver','Ended')
END SUBROUTINE PySpiceCouplerSolver








