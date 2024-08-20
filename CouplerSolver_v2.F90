
SUBROUTINE CouplerSolver_Init( Model,Solver,dt,TransientSimulation)
!------------------------------------------------------------------------------
    USE DefUtils
    IMPLICIT NONE
!------------------------------------------------------------------------------
    TYPE(Model_t)  :: Model
    TYPE(Solver_t), TARGET :: Solver
    LOGICAL ::  TransientSimulation
    REAL(KIND=dp) :: dt
!------------------------------------------------------------------------------
    !------------------------------------------------------------------------------
    LOGICAL :: Found, Calculate
    TYPE(ValueList_t), POINTER :: Params
    CHARACTER(LEN=MAX_NAME_LEN) :: VariableName
    INTEGER :: Dim
    CALL Info('CouplerSolver','Initializing',LEVEL=4)
    Params => GetSolverParams()
    ! Print *, 'Read data name = ', ListGetString(Params, 'readDataName', Found)
    CALL Info( 'CouplerSolver Data from preCICE', ListGetString(Params, 'readDataName', Found), LEVEL=4 )
    ! WRITE( Message, '(a,I0)' ) 'Write data name = ', ListGetString(Params, 'writeDataName', Found)
    CALL Info( 'CouplerSolver Data to preCICE', ListGetString(Params, 'writeDataName', Found), LEVEL=4 )
    ! Print *, 'Write data name = ', ListGetString(Params, 'writeDataName', Found)
    ! print the lenght of the list
    ! print *, 'Length of the list = ', SIZE(Params)
    Dim = CoordinateSystemDimension()
    CALL Info('CouplerSolver Coordinate System Dimension = ', TRIM(I2S(Dim)), LEVEL=4)
END SUBROUTINE CouplerSolver_Init


SUBROUTINE CouplerSolver( Model,Solver,dt,TransientSimulation)
!------------------------------------------------------------------------------
    USE DefUtils
    USE Differentials
    IMPLICIT NONE
!------------------------------------------------------------------------------ 
    TYPE(Model_t) :: Model
    TYPE(Solver_t), TARGET:: Solver
    REAL (KIND=DP) :: dt
    LOGICAL :: TransientSimulation
!------------------------------------------------------------------------------
!    Local variables
!------------------------------------------------------------------------------
    TYPE(Matrix_t), POINTER  :: StiffMatrix
    TYPE(Element_t), POINTER :: CurrentElement
    TYPE(Nodes_t) :: ElementNodes

    REAL (KIND=DP), POINTER :: ForceVector(:), Potential(:)
    REAL (KIND=DP), POINTER :: ElField(:), VolCurrent(:)
    REAL (KIND=DP), POINTER :: Heating(:), NodalHeating(:)
    REAL (KIND=DP), POINTER :: EleC(:)
    REAL (KIND=DP), POINTER :: Cwrk(:,:,:)
    REAL (KIND=DP), ALLOCATABLE ::  Conductivity(:,:,:), &
      LocalStiffMatrix(:,:), Load(:), LocalForce(:)

    REAL (KIND=DP) :: Norm, HeatingTot, VolTot, CurrentTot, ControlTarget, ControlScaling = 1.0
    REAL (KIND=DP) :: Resistance, PotDiff
    REAL (KIND=DP) :: at, st, at0

    INTEGER, POINTER :: NodeIndexes(:)
    INTEGER, POINTER :: PotentialPerm(:)
    INTEGER :: i, j, k, n, t, istat, bf_id, LocalNodes, Dim, &
        iter, NonlinearIter

    LOGICAL :: AllocationsDone = .FALSE., gotIt, FluxBC
    LOGICAL :: CalculateField = .FALSE., ConstantWeights
    LOGICAL :: CalculateCurrent, CalculateHeating, CalculateNodalHeating
    LOGICAL :: ControlPower, ControlCurrent, Control

    TYPE(ValueList_t), POINTER :: Params
    TYPE(Variable_t), POINTER :: Var

    CHARACTER(LEN=MAX_NAME_LEN) :: EquationName

    LOGICAL :: GetCondAtIp
    TYPE(ValueHandle_t) :: CondAtIp_h
    REAL(KIND=dp) :: CondAtIp
    
    SAVE LocalStiffMatrix, Load, LocalForce, &
         ElementNodes, CalculateCurrent, CalculateHeating, &
         AllocationsDone, VolCurrent, Heating, Conductivity, &
         CalculateField, ConstantWeights, &
         Cwrk, ControlScaling, CalculateNodalHeating

!------------------------------------------------------------------------------
!    Get variables needed for solution
!------------------------------------------------------------------------------
    Potential     => Solver % Variable % Values
    PotentialPerm => Solver % Variable % Perm    
    Params => GetSolverParams()
    
    LocalNodes = Model % NumberOfNodes
    StiffMatrix => Solver % Matrix
    ForceVector => StiffMatrix % RHS

    Norm = Solver % Variable % Norm
    DIM = CoordinateSystemDimension()

    IF(.NOT.ASSOCIATED(Solver % Matrix)) RETURN


    IF ( .NOT. AllocationsDone .OR. Solver % MeshChanged ) THEN
        N = Model % MaxElementNodes
  
        IF(AllocationsDone) THEN
          DEALLOCATE( ElementNodes % x, &
                    ElementNodes % y,   &
                    ElementNodes % z,   &
                    Conductivity,       &
                    LocalForce,         &
                    LocalStiffMatrix,   &
                    Load )
        END IF
 
        ALLOCATE( ElementNodes % x(N),   &
                  ElementNodes % y(N),   &
                  ElementNodes % z(N),   &
                  Conductivity(3,3,N),   &
                  LocalForce(N),         &
                  LocalStiffMatrix(N,N), &
                  Load(N),               &
                  STAT=istat )
  
        IF ( istat /= 0 ) THEN
          CALL Fatal( 'StatCurrentSolve', 'Memory allocation error.' )
        END IF
 
        NULLIFY( Cwrk )

        AllocationsDone = .TRUE.
    END IF

    CALL Info( 'CouplerSolver', '-------------------------------------',Level=4 )
    CALL Info( 'CouplerSolver', 'Coupler Solver:  ', Level=4 )
    CALL Info( 'CouplerSolver', '-------------------------------------',Level=4 )

    DO t=Solver % Mesh % NumberOfBulkElements + 1, &
        Solver % Mesh % NumberOfBulkElements + &
        Solver % Mesh % NumberOfBoundaryElements
        
        CurrentElement => Solver % Mesh % Elements(t)

        DO i=1,Model % NumberOfBCs
            IF ( CurrentElement % BoundaryInfo % Constraint == &
               Model % BCs(i) % Tag ) THEN
                Model % CurrentElement => CurrentElement

                n = CurrentElement % TYPE % NumberOfNodes
                NodeIndexes => CurrentElement % NodeIndexes

                ! IF ( ANY( PotentialPerm(NodeIndexes) <= 0 ) ) CYCLE  ! This causes a segfault
                
                FluxBC = ListGetLogical(Model % BCs(i) % Values, &
                'Current Density BC',gotIt) 
                
                IF(GotIt .AND. .NOT. FluxBC) CYCLE
                
                IF (ListGetString(Model % BCs(i) % Values, 'Name', Found) /= 'r1_1') CYCLE 

                 !------------------------------------------------------------------------------
                !             BC: cond@Phi/@n = g
                !------------------------------------------------------------------------------
                ! Load = 0.0d0
                ! Load(1:n) = ListGetReal( Model % BCs(i) % Values,'Current Density', &
                !     n,NodeIndexes,gotIt )
                ! IF(.NOT. GotIt) CYCLE
                
                ! Print *, 'Load = ', Load

                Print *, 'BC name : ', GetReal( Model % BCs(i) % Values,'Current Density', GotIt ) 
                
                ElementNodes % x(1:n) = Solver % Mesh % Nodes % x(NodeIndexes)
                ElementNodes % y(1:n) = Solver % Mesh % Nodes % y(NodeIndexes)
                ElementNodes % z(1:n) = Solver % Mesh % Nodes % z(NodeIndexes)

                
            END IF

        END DO
    END DO
END SUBROUTINE CouplerSolver








