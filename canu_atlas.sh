 #running Canu on Atlas

#Canu is installed as a conda package at the location /project/molecular_epidemiology/conda_envs/canu

#Atlas temp storage
TMPDIR=/local/scratch/$SLURM_JOB_USER/$SLURM_JOB_ID


# Always good practice to reset environment when you start
 module purge

 # start staging data to the job temporary directory in $TMPDIR
 MYDIR=`pwd`
 /bin/cp -r $MYDIR $TMPDIR/
 cd $TMPDIR

 # add regular job commands like module load
 # and commands to launch scientific software

 # copy output data off of local scratch
 /bin/cp -r output $MYDIR/output
