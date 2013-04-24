from systemfuncs import systemFunctions
from outpututils import rewriteIfChanged

import sys

def iterSystemFuncsHeader(functionResults):
	yield '// Automatically generated by build system.'
	for makeName in sorted(
		func.getMakeName() for func in systemFunctions
		):
		yield '#define HAVE_%s %d' % (makeName, functionResults[makeName])

def getSystemFuncsInfo():
	return dict.fromkeys(
		(func.getMakeName() for func in systemFunctions),
		False
		)

if __name__ == '__main__':
	if len(sys.argv) == 2:
		rewriteIfChanged(
			sys.argv[1],
			iterSystemFuncsHeader(getSystemFuncsInfo())
			)
	else:
		print >> sys.stderr, \
			'Usage: python systemfuncs2code.py CONFIG_HEADER '
		print >> sys.stderr, \
			'Note: Should only be called directly on systems where the probe ' \
			'does not work.'
		sys.exit(2)
