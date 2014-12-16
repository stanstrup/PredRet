# Auto-translate

pubchem=CTSgetR('isoleucine',from='Chemical Name',to='PubChem CID')

pubchem_info = get.cid(temp[,'PubChem.CID'])

inchi = smile2inchi(pubchem_info[,'CanonicalSmiles'])

sdf = inchi2sdf(inchi)

plotStruc(sdf[[1]])