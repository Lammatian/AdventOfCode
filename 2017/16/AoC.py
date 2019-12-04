with open("input.txt") as f:
	moves = f.read().split(',')

p = [chr(x) for x in range(97, 113)]
check = [chr(x) for x in range(97, 113)]

for i in range(48):
	for m in moves:
		# swap
		if m[0] == 's':
			p = p[-int(m[1:]):] + p[:-int(m[1:])]
		elif m[0] == 'x':
			a, b = map(int, m[1:].split('/'))
			p[a], p[b] = p[b], p[a]
		elif m[0] == 'p':
			a, b = m[1:].split('/')
			a = p.index(a)
			b = p.index(b)
			p[a], p[b] = p[b], p[a]

	print(''.join(p))

# ehdpincaogkblmfj
# dmlnbocgjeihafpk
# dnmekljcfoaibphg
# gfhdmlneckpaojbi
# bkpomeajhcfglnid
# nmagofilhdjbkecp
# jcnmibgakldoefhp
# eobnfglajmpihkdc
# mpfneoadihlcbjkg
# nfilkhmeopcgbdaj
# hfjopbdgcanlmeik
# lhgefpbcanjokmid
# fcedamglibjhokpn
# ekomhigbnfaclpdj
# emkfjongpilahdcb
# bpcekomfgjdlinha
# hjdikflncgpbomae
# mklbipaocenhjfgd
# ngmkahbljoeifpcd
# fihmpbolnkdacjeg
# kdpmfileacoghnjb
# mpaojckfidgbheln
# cpnidhebglmokfaj
# ocbfpdhglmnijkae
# pgfelkboahncijdm
# fjikcabhmplgoden
# fkjpnimbdaolcegh
# hdgfjikpbneoamcl
# cneajpomgbdhiklf
# kjohadligfmcnpbe
# mbkjlchonifapdge
# packdhiomjelgnfb
# jedkpaoflgibcmnh
# kdlingjpaebhcfom
# gdmaecfhbokijpln
# ighpdecbokmanjlf
# dbpfojhilcmganek
# pnajglhckdobiefm
# pjndmakheliogfbc
# cebpnajdhmfilkgo
# gmflndikbhecajop
# jnicleoabpkgmdhf
# khjnogcimapldebf
# dlgjecaiknfobmph
# nfejdlipobahgkmc
# jeoambndlfhcgpik
# beklfgpchijandom
# abcdefghijklmnop