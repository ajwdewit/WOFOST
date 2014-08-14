** $Id: dimens.inc 1.3 1997/10/02 15:34:14 LEM release $
*-----Include block 'DIMENS.INC'

*-----Maximum number of harvests (sampling times)
      INTEGER IMXHVS
      PARAMETER (IMXHVS=53)

*-----Maximum number of state-variables in the optimization
      INTEGER IMXNDP
      PARAMETER (IMXNDP=3)

*-----Maximum number of experimental data-sets
      INTEGER IMXNDS
      PARAMETER (IMXNDS=25)

*-----Maximum number of rerun-sets in optimization
      INTEGER IMXNRR
c WSt PARAMETER (IMXNRR=1500)
      PARAMETER (IMXNRR=600)

*-----Maximum number of replicates per observation
      INTEGER IMXREP
      PARAMETER (IMXREP=5)
