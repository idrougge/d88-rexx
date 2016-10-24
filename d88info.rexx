/* REXX */
/* Read NEC PC-8801 D88 format disk images */
options AREXX_BIFS
options AREXX_SEMANTICS
filename=arg(1)
if filename='' then filename='/Users/iggy/Downloads/NEC PC 8801 [TOSEC]/NEC PC-8801 - Applications (TOSEC-v2007-08-30_CM)/NEC PC-8801FE N88 BASIC v2.3 (1988)(Microsoft).d88'
say 'fil:' filename
if ~open(file,filename,'READ') then do
	say 'Kunde inte öppna fil!'
	exit 10
end
header=readch(file,32)
say 'Läste header på' length(header) 'byte:'
say hexstr(header)
parse value header with label +17 . +9 writeprotect +1 disktype +1 disksize +4 .
if c2d(label)=0 then label='ej satt'
say 'Disknamn:' label
if writeprotect='10'x then say 'Skrivskyddad'
if writeprotect='00'x then say 'Ej skrivskyddad'
/* 1D = SS/DD 40 track   2D = DS/DD 40 track   2DD = DS/DD 80 track   2HD = DS/HD 80 track */
disktypes.='FEL'; disktypes.00='2D'; disktypes.10='2DD'; disktypes.20='2HD'
densities.='FEL'; densities.00='DD'; densities.40='SD'
disktype=c2x(disktype)
say 'Typ av diskett:' disktypes.disktype
disksize=revendian(disksize)
say 'Diskens storlek:' c2d(disksize) '$'c2x(disksize)

fat.=''
call parsefat getfat()

files.=''
call readdir

call readfile 2

exit

readfile:
file#=arg(1)
cluster#=files.file#.cluster
say 'Läser fil nr' file#':' files.file#.name 'med start på cluster' cluster#
clusterptr=fat.cluster#
/* do while cluster# < 'C0' */
do while clusterptr < 'C0'
/* do while fat.cluster# < 'C0' */
	say 'cluster#='cluster#
	clusterptr=fat.cluster#
	say 'clusterptr='clusterptr
	call readcluster cluster#,8
	/* call readcluster clusterptr,8 */
	/* clusterptr=fat.clusterptr */
	
	cluster#=fat.cluster#
end
clusterptr=fat.cluster#
say 'clusterptr='clusterptr
if left(clusterptr,1)='C' then do
	len=right(clusterptr,1)
	call readcluster clusterptr,len
end
return
call seeksector 1
do j=1 to sectorcount
	say 'j='j
	call seeksector j
	call getsector
end
return

readcluster:
/* cluster#=x2d(arg(1)) */
len=arg(2)-1
parse value cluster2physical(x2d(arg(1))) with track# sector#
endsector=sector#+len
say 'readcluster: cluster' arg(1)', sectors' sector# '-' endsector
say 'readcluster: track' track# '($'d2x(track#)'), sector' sector#
call seektrack track#
call getsector QUIET
call seeksector sector#
call dumpsector
return

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

readfat:
fats.='ERR'; fats.FF='TOM'; fats.FE='SYS'
call getsector
sector=readch(file,sectorsize)
cluster#=0
dumpsector
return

readfat__:
fats.='ERR'; fats.FF='TOM'; fats.FE='SYS'
call getsector
sector=readch(file,sectorsize)
cluster#=0
do sectorsize
	parse var sector cluster +1 sector
	cluster=c2x(cluster)
	select
		when fats.cluster='TOM' then do
			say 'Clusternummer' '$'d2x(cluster#) 'tomt'
			fat.cluster#=fats.cluster
			cluster#=cluster#+1
		end
		when fats.cluster='SYS' then do
			say 'Clusternummer' '$'d2x(cluster#) 'reserverat'
			fat.cluster#=fats.cluster
			cluster#=cluster#+1
		end
		otherwise do
			select
				when cluster<='9F' then do
					say 'Clusternummer:' '$'cluster
					cluster#=x2d(cluster)
				end
				when cluster>='C1' | cluster <= 'C8' then do
					say 'Filslut:' '$'cluster '('right(cluster,1) 'sektorer)'
				end
				otherwise say 'Ogiltigt clustervärde:' '$'cluster
			end
		end
	end
end
return

readdir:
call seektrack fattrack
call seeksector 1
call getsector
legalcharacters=xrange('a','z') || xrange(0,9) || ' ' || '@'
attribs.='FEL'; attribs.00='ASC'; attribs.01='BIN'; attribs.80='BAS'; attribs.10='WRP'; attribs.20='RDP'; attribs.40='RAW'
do #=1
	entry=readch(file,16)
	parse value entry with fn +6 ext +3 attr +1 cluster# +1 .
	if ~datatype(strip(fn)||strip(ext),'ALPHA') then nop
	if verify(fn||ext,legalcharacters) > 0 then do
		say 'Ogiltigt filnamn:' hexstr(fn||ext)
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

gettrackoffset: procedure
track#=arg(1)
say 'track#='track#
call seek file,32,'BEGIN'
call seek file,track#*4
offset=readch(file,4)
say 'Hämtade trackoffset för spår' arg(1)':' c2d(revendian(offset)) '('hexstr(offset)')'
return c2d(revendian(offset))

getsectoroffset: procedure expose sectorsize trackoffset sectorcount
if arg(1) = 0 then do
	say 'Sektornummer börjar på 1!'
	return
end
if arg(1) > sectorcount then do
	say 'Sektornummer för högt!'
	return
end
sector# = arg(1) - 1
say 'Hämtar offset till sektor' arg(1)
call seek file,trackoffset,'BEGIN'
return (16+sectorsize)*sector# 

/*
	Jump to track
*/
seektrack:
/* En 2D-disk har bara 80-84 spår, 2DD och 2HD 160-164 spår. */
trackoffset=gettrackoffset(arg(1))
say 'Söker till position' trackoffset seek(file,trackoffset,'BEGIN')
return

seeksector:
say 'Söker till sektor' arg(1)
call seek file,getsectoroffset(arg(1))
return

dumpsector:
say 'Dumpar sektor:'
say hexstr( readsector() )
return

readsector:
call getsector QUIET
return readch(file,sectorsize)

/*
	Read sector and show information
*/
getsector:
header=readch(file,16)
parse value header with c +1 h +1 r +1 n +1 sectorcount +2 density +1 deleted +1, 
	status +1 . +5 sectorsize +2 .
sectorcount=c2d(revendian(sectorcount))
density=c2x(density)
sectorsize=c2d(revendian(sectorsize))
if arg(1)=='QUIET' then return
say 'Spårheader:' hexstr(header)
say 'C:' hexstr(c) 'H:' hexstr(h) 'R:' hexstr(r) 'N:' hexstr(n)
say 'Spår:' c2d(c) 'Huvud:' c2d(h) 'Sektor:' c2d(r) 'Sektorlängd:' c2d(n)
say 'Antal sektorer:' sectorcount
say 'Densitet:' densities.density
say 'Datastorlek:' sectorsize hexstr(revendian(d2c(sectorsize)))
say 'Spårets längd:' sectorcount*sectorsize 'byte ('sectorcount'*'sectorsize')'
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