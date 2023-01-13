# pf 0.1.4

* new function projects_ctu_report added (DG)

# pf 0.1.3

* option to pass options through to downloadPFdata added to getPFdata (e.g. `all = TRUE`)

# pf 0.1.2

* add contactfield to data exported

# pf 0.1.1

* bug fix in getPFData

# pf 0.1.0

* first version of quarterly report

# pf 0.0.9

* getPDData gives a message of when data was last modified

# pf 0.0.8

* projectSize function added to estimate project size based on planned budget

# pf 0.0.7

* CaseId (e.g. P-1074) returned by constructProjectParents

# pf 0.0.6

* bug fix in decodeCustomFields

# pf 0.0.5

* addition of function prepTime

# pf 0.0.4

* constructCustomerParents now includes divisions

# pf 0.0.3

* refactored constructProjectParents to use constructParents
* function for ODBC download (downloadPFdata). this function also sets the string encoding. it also removes non-CTU records in tables.
* refactorization of getPFData to use the ODBC function
* savePFdata function for saving the files downloaded
* discard_all renamed discard_all_NA

# pf 0.0.2

* added constructParents (a more generic) and refactored constructCustomerParents to use it instead

# pf 0.0.1

* added constructCustomerParents function
* decodeCustomFields added

# pf_app 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
