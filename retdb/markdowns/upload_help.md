Here you can upload a **csv** file containing your data. The spreadsheet should contain the following named columns:

 - **Compound**: The name of the compound. (*column is mandatory*)
 - **rt**: The retention time of the compound in your system. Minutes are the preferred unit. (*column is mandatory*)
 - **Method**: The name of the chromatographic method. The method just first be described in the "add or modify system" tab. (*this column is mandatory unless a method name is selected above*)
 - **Pubchem**: The pubchem cid of the compound. (*Either pubchem or InChi must be supplied for each compound*)
 - **Inchi**: The InChi of the compound. (*Either pubchem or InChi must be supplied for each compound*)

The system will automatically convert pubchem cids to InChis for unambiguous comparision of molecular structures.
