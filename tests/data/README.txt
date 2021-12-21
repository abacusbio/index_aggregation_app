1] 
Test file naming conventions

The naming conventions for data files is type_testtype_feature.ext. E.g For a file named ev_dup_header.csv :
ev = File type being tested.
dup = Test is for duplication.
header = Testing for duplicate headers.

Use underscores if each of the subparts have a compound word. E.g For desc_bv_missing_order.csv, desc_bv is the file type being tested.

2] 
Test File Catalogue

a]
BV file description
desc_bv.xlsx	: Normal'good'data.Has just the required columns and values.
desc_bv_op.csv	: Normal'good'data but with optional fields.
desc_bv_dup_header.xlsx	: Has duplicate column headers.
desc_bv_mismatch_acc_trait.xlsx : ACC-Trait mismatch. Has ACC classifier entries but does not have the corresponding TraitACC column_labels.
desc_bv_identical_order.csv	: Has identical order values in trait label/EBV rows.
desc_bv_missing_order.csv	: Has missing order values in trait label/EBV rows.
desc_bv_missing_group.csv	: Has missing group values in trait label/EBV rows.

b]
EV file description
desc_ev.csv	: Normal'good'data.Has just the required columns and values.
desc_ev_mismatch_trait.csv	: Column_labelling contents are not identical to those in the EBV description file.
desc_ev_match.csv		: Description file with matching trait (column_labelling) entries with ev.

c]
BV file
bv.csv	: Normal'good'data.Has just the required columns and values.

d]
EV file
ev.csv	: Normal'good'data.Has just the required columns and values.
ev_dup_header.csv	: 'Index' column is duplicated.
ev_dup_index.csv	: Indices are not unique.
ev_mismatch_header.csv	: List of column headers in the description file do not match with thoses in this data file.
ev_invalid_header.csv	: One or more column header names do not exist in the description file

e]
Economic weights data
index_wt.csv	: Normal'good'data.Has just the required columns and values.
index_dup_wt.csv	: Indices are not unique
index_mismatch_wt.csv	: Index name(s) does not exist in economic value file.


