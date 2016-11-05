/* REXX */
/* Read NEC PC-8801 D88 format disk images */
options AREXX_BIFS
options AREXX_SEMANTICS
filename=arg(1)
call init

call readfile 1

exit

/*
	Read a file and write it to disk
	Parameters: file number
*/
readfile:
file#=arg(1)
call open outfile,files.file#.name,'WRITE'
cluster#=files.file#.cluster
say 'Reading file #' file#':' files.file#.name 'starting at cluster' cluster#
clusterptr=fat.cluster#
do while fat.cluster# < 'C0'
	say 'clusterloop'
	say 'cluster#='cluster#
	clusterptr=fat.cluster#
	say 'clusterptr='clusterptr
	call dumpcluster cluster#,8
	cluster#=fat.cluster#
end
say 'cluster#='cluster#
clusterptr=fat.cluster#
say 'clusterptr='clusterptr
if left(clusterptr,1)='C' then do
	len=right(clusterptr,1)
	say 'len:' len
	call dumpcluster cluster#,len
end
call close outfile
return

/*
	Read a cluster and display on console as hex
*/
dumpcluster:
say 'cluster' arg(1)':'
say hexstr(readcluster(arg(1),arg(2)))
return

/*
	Read and return a cluster while writing it to a file
	Parameters: cluster number, cluster size
*/
readcluster:
len=arg(2)
parse value cluster2physical(x2d(arg(1))) with track# sector#
endsector=sector#+len-1
say 'readcluster: cluster' arg(1)', sectors' sector# '-' endsector
sector=''
do sector#=sector# for len
say 'readcluster: track' track# '($'d2x(track#)'), sector' sector#
call seektrack track#
call getsector QUIET
call seeksector sector#
sector = sector || readsector()
end
say 'wrote' writech(outfile,sector) 'bytes'
return sector

/*
	Translate cluster number to track and sector
*/
cluster2physical: procedure
/* Ett cluster = 8 sektorer
   Ett spår = 16 sektorer
   Cluster 0 = spår 0, sektor 1-8
   Cluster 1 = spår 0, sektor 9-16
   Cluster 2 = spår 1, sektor 1-8
   o s v ... 
   Om disketten är dubbelsidig
   är spår 0 = spår 0, sida 0
   och spår 1 = spår 0, sida 1
   */
cluster = arg(1)
track = cluster % 2
/* track = shiftright(cluster) */
sector = 8 * (cluster // 2) + 1
return track sector

/* 
Locate and read FAT for parsing by parsefat procedure
*/
getfat:
/* FAT is located on track 18 (12h), head 1, which equals track 37. 
   On 2HD disks, it seems to be track 35 instead. */
if disktypes.disktype='2D'  then fattrack=37
if disktypes.disktype='2HD' then fattrack=35
call seektrack fattrack
call getsector
call seeksector 14
return readsector()

/*
	Parse FAT table into "fat." stem
*/
parsefat: procedure expose fat.
fat=arg(1)
/* say 'FAT:' hexstr(fat) */
do i=0 to 255
	parse var fat nr +1 fat
	j=right(d2x(i),2,0)
	fat.j=c2x(nr)
end
return

/*
	Read and display directory
*/
readdir:
call seektrack fattrack
call seeksector 1
call getsector
legalcharacters=xrange('a','z') || xrange(0,9) || ' ' || '@'
attribs.='ERR'; attribs.00='ASC'; attribs.01='BIN'; attribs.80='BAS'; attribs.10='WRP'; attribs.20='RDP'; attribs.40='RAW'
do #=1
	entry=readch(file,16)
	parse value entry with fn +6 ext +3 attr +1 cluster# +1 .
	if ~datatype(strip(fn)||strip(ext),'ALPHA') then nop
	if verify(fn||ext,legalcharacters) > 0 then do
		say 'Illegal filename:' hexstr(fn||ext)
		return
	end
	attr=c2x(attr)
	attrib=attribs.attr
	track#=shiftright(cluster#)
	say 'Fil' #':' fn'.'ext 'Attribut:' attrib '($'attr')' 'Cluster:' hexstr(cluster#) 'Spår:' hexstr(track#)
	files.#.name=fn'.'ext
	files.#.attributes=attr
	files.#.cluster=c2x(cluster#)
/*
	call readtrack(gettrackoffset(c2d(track#)))
	leave
*/
end
return

/*
	Get track offset from beginning of file
	Offsets are stored in a table beginning at byte 32.
	Each entry is a little-endian longword.
*/
gettrackoffset: procedure
track#=arg(1)
say 'track#='track#
call seek file,32,'BEGIN'
call seek file,track#*4
offset=readch(file,4)
say 'Fetch offset for track' arg(1)':' c2d(revendian(offset)) '('hexstr(offset)')'
return c2d(revendian(offset))

/*
	Get sector offset from beginning of file
*/
getsectoroffset: procedure expose sectorsize trackoffset sectorcount
if arg(1) = 0 then do
	say 'Sector numbers begin at 1, not 0!'
	return
end
if arg(1) > sectorcount then do
	say 'Sector number too high!'
	return
end
sector# = arg(1) - 1
say 'Fetch offset for sector' arg(1)
call seek file,trackoffset,'BEGIN'
return (16+sectorsize)*sector# 

/*
	Jump to track
*/
seektrack:
/* En 2D-disk har bara 80-84 spår, 2DD och 2HD 160-164 spår. */
trackoffset=gettrackoffset(arg(1))
say 'Seek to position' trackoffset seek(file,trackoffset,'BEGIN')
return

/*
	Jump to sector
*/
seeksector:
say 'Seek to sector' arg(1)
call seek file,getsectoroffset(arg(1))
return

/*
	Dump sector to console as hex
*/
dumpsector:
say 'Dumping sector:'
say hexstr( readsector() )
return

/*
	Read and return sector data
*/
readsector:
call getsector QUIET
return readch(file,sectorsize)

/*
	Read sector header and show information
*/
getsector:
header=readch(file,16)
parse value header with c +1 h +1 r +1 n +1 sectorcount +2 density +1 deleted +1, 
	status +1 . +5 sectorsize +2 .
sectorcount=c2d(revendian(sectorcount))
density=c2x(density)
sectorsize=c2d(revendian(sectorsize))
if arg(1)=='QUIET' then return
say 'Sector header:' hexstr(header)
say 'C:' hexstr(c) 'H:' hexstr(h) 'R:' hexstr(r) 'N:' hexstr(n)
say 'Track:' c2d(c) 'Head:' c2d(h) 'Sector:' c2d(r) 'Sector length:' c2d(n)
say '# of sectors:' sectorcount
say 'Density:' densities.density
say 'Data size:' sectorsize hexstr(revendian(d2c(sectorsize)))
say 'Track length:' sectorcount*sectorsize 'bytes ('sectorcount'*'sectorsize')'
return

/*
	Open image file and read disk information
*/
init:
if filename='' then filename='/Users/iggy/Downloads/NEC PC 8801 [TOSEC]/NEC PC-8801 - Applications (TOSEC-v2007-08-30_CM)/NEC PC-8801FE N88 BASIC v2.3 (1988)(Microsoft).d88'
if ~open(file,filename,'READ') then do
	say 'Could not open file!'
	exit 10
end
header=readch(file,32)
say 'Reading header of' length(header) 'bytes:'
say hexstr(header)
parse value header with label +17 . +9 writeprotect +1 disktype +1 disksize +4 .
if c2d(label)=0 then label='NONE'
say 'Disk label:' label
if writeprotect='10'x then say 'Write protected'
if writeprotect='00'x then say 'Not protected'
/* 1D = SS/DD 40 track   2D = DS/DD 40 track   2DD = DS/DD 80 track   2HD = DS/HD 80 track */
disktypes.='ERR'; disktypes.00='2D'; disktypes.10='2DD'; disktypes.20='2HD'
densities.='ERR'; densities.00='DD'; densities.40='SD'
disktype=c2x(disktype)
say 'Type of disk:' disktypes.disktype
disksize=revendian(disksize)
say 'Disk size:' c2d(disksize) '$'c2x(disksize)

/* FAT magic numbers */
fats.='ERR'; fats.FF='___'; fats.FE='SYS'
/* Initialise FAT table */
fat.=''
call parsefat getfat()
/* Read directory */
files.=''
call readdir
return

/* Endian swap */
revendian: procedure
parse value arg(1) with a +1 b +1 c +1 d+1 .
return d || c || b || a

/* Prettyprint char as hex */
hexstr: procedure
in=c2x(arg(1))
out='$'
do while in~=''
	parse var in temp +4 in
	out = out || temp || ' '
end
return strip(out)

/* LSL */
shiftleft: procedure
binstr=c2b(arg(1))
binstr = binstr || '0'
return b2c(binstr)

/* LSR */
shiftright: procedure
binstr=c2b(arg(1))
binstr = '0' || left(binstr,length(binstr)-1)
return b2c(binstr)

/* ROR */
rotateright: procedure
binstr=c2b(arg(1))
binstr = right(binstr,1) || left(binstr,length(binstr)-1)
return b2c(binstr)