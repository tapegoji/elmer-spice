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
        CALL Info('CouplerSolver','Printing ' //TRIM(dataName))
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
            CALL Info('CouplerSolver',infoMessage)

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

        CALL Info('CouplerSolver','Printing ' //TRIM(dataName))
        DO i = 1, mesh % NumberOfNodes
            
            
            write(infoMessage,'(A,I5,A,F10.4,A,F10.6,A,F10.6)') 'Node: ',i,' Value: ' &
                            ,dataVariable % Values(dataVariable % Perm(i)),&
                            ' X= ', mesh % Nodes % x(i), ' Y= ', mesh % Nodes % y(i) 
                                                          
            CALL Info('CouplerSolver',infoMessage)

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
            CALL Info('CouplerSolver','Using existing variable : '//TRIM(dataName) )
        ELSE
            CALL FATAL('CouplerSolver', 'Variable does not exist : ' // TRIM(dataName) )
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



SUBROUTINE CouplerSolver( Model,Solver,dt,TransientSimulation)

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
    TYPE(ValueList_t), POINTER          :: simulation, solverParams ! Simulation gets Simulation list, & solverParams hold solver1,solver 2,etc
    TYPE(Valuelist_t), POINTER :: BC
    !------------------------Data Arrays----------------------------------------------
    REAL(KIND=dp), POINTER              :: CoordVals(:)
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
    INTEGER, POINTER                :: vertexIDs(:)
    REAL(KIND=dp) :: dmax
    !------------------------Mesh Data----------------------------------------------
    INTEGER                         :: BoundaryNodes
    INTEGER                         :: Dofs
    INTEGER                         :: dimensions ! ?? Do not know 
    !----------------------Time Loop Control Variables-----------------------------
    INTEGER                         :: bool
    INTEGER                         :: ongoing
    INTEGER                         :: i,j
    !--------------------------Variables-End-------------------------------------------

    REAL (KIND=DP), ALLOCATABLE ::  Load(:)

    !--------------------------SAVE-Start-------------------------------------------
    SAVE meshName,readDataName,writeDataName
    SAVE itask
    SAVE BoundaryPerm,CoordVals,vertexIDs
    SAVE readData,writeData
    SAVE BoundaryNodes
    SAVE dmax
    SAVE Load
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
        CALL Info('CouplerSolver ', 'Initializing Coupler Solver')
        !--- First Time Visited, Initialization
        !-- First Time Visit, Create preCICE, create nodes at interface
        !----------------------------- Initialize---------------------
        
        !----------------Acquire Names for solver---------------------
        ! BoundaryName = GetString( Simulation, 'maskName', Found )
        BoundaryName = 'r1_1'
        participantName = GetString( Simulation, 'participantName', Found )
        
        !-----------------Convert to preCICE Naming Convention    
        IF (participantName == 'solid') THEN
            participantName = 'Solid'
        END IF
        IF (participantName == 'fluid') THEN
            participantName = 'Fluid'
        END IF
        meshName = GetString( Simulation, 'meshName', Found )
        IF (meshName == 'solid-mesh') THEN
            meshName = 'Solid-Mesh'
        END IF
        IF (meshName == 'fluid-mesh') THEN
            meshName = 'Fluid-Mesh'
        END IF
        !---------------------------------------------------------------------

        !-----------------Get Config Path-------------------------------------
        config = GetString( Simulation, 'configPath', Found )

        Print *, TRIM(BoundaryName)," ",TRIM(participantName)," ",TRIM(meshName)," ",TRIM(config)
        
        !-----------Identify Vertex on Coupling Interface & Save Coordinates--------------------
        NULLIFY( BoundaryPerm )    
        ALLOCATE( BoundaryPerm( Mesh % NumberOfNodes ) )
        BoundaryPerm = 0
        BoundaryNodes = 0
        CALL MakePermUsingMask( Model,Model%Solver,Model%Mesh,BoundaryName,.FALSE., &
            BoundaryPerm,BoundaryNodes)


        CALL Info('CouplerSolver','Boundary Name: '//TRIM(BoundaryName))
            
        CALL Info('CouplerSolver','Number of nodes at interface:'//TRIM(I2S(BoundaryNodes)))
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
        ALLOCATE(writeData(BoundaryNodes*meshDim))
        ALLOCATE(Load(BoundaryNodes))
        

        ! Print the type of BoundaryPerm
        DO i=1,Model % NumberOfBCs
            ! Print *, 'BC: ',  ListGetLogical(Model % BCs(i) % Values, &
            ! 'Current Density BC',Found)  ! this works
            
            IF(.NOT. ListGetLogical(Model % BCs(i) % Values, &
                'Current Density BC',Found)) CYCLE ! this works too
            BC => Model % BCs(i) % Values
            Load = 0.0d0
            IF (ListGetString(BC, 'Name', Found) /= BoundaryName) CYCLE
            CALL ListAddConstReal(BC, 'Current Density', -55555555.1d0) 
            ! Print *, 'BC name : ', GetReal(BC,'Current Density', Found) ! this works, gives the value for the whole boundary
            ! Print *, 'BC: ',  ListGetReal( Model % BCs(i) % Values,'Current Density', &
            !     BoundaryNodes ,vertexIDs,Found ) ! this works. gives the value at each node
        END DO



        CALL Info('CouplerSolver','Created nodes at interface')  

        ! !-----------Identify read and write Variables and Create it if it does not exist--------------------
        ! CALL CreateVariable(readDataName,'readDataName',mesh,BoundaryPerm,Solver,solverParams)
        CALL CreateVariable(writeDataName,'writeDataName',mesh,BoundaryPerm,Solver,solverParams)
        !-----------------------------------------------------------------------------------------

        itask = 2
    case(2)
        ! Place holder for readdata. i.e setting the current density in elmer
        !-----------------------------------------------------------------------------------------

        itask = 3
    case(3)

        !-------------------Copy Write values from Variable to buffer----------------------------
        CALL Info('CouplerSolver','Writing data to circuit simulator')

        DO i = 1, 1
            BoundaryName = 'R' // TRIM(I2S(i))// '_1'
            NULLIFY( BoundaryPerm )    
            ALLOCATE( BoundaryPerm( Mesh % NumberOfNodes ) )
            BoundaryPerm = 0
            BoundaryNodes = 0
            CALL MakePermUsingMask( Model,Model%Solver,Model%Mesh,BoundaryName,.FALSE., &
                BoundaryPerm,BoundaryNodes)
            CALL GetMaxData(writeDataName,mesh,BoundaryPerm,dmax)
            print *, 'Maximum ', TRIM(writeDataName), ' at ', TRIM(BoundaryName), ' = ', dmax

        END DO
        itask = 2
    end select

    CALL Info('CouplerSolver','Ended')
END SUBROUTINE CouplerSolver








