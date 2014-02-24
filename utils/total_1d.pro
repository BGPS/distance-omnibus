;+
; NAME:
;	TOTAL_1D
;
; COPYRIGHT:
;	Copyright (1999) Myles Allen, Space Science Department, 
;	Rutherford Appleton Laboratory.
; 	Prepared under contract to the Hadley Centre for Climate Prediction 
;	and Research.
;
; PURPOSE:
;	This function is a manual implementation of the IDL total function to 
;	get around the fact that PV-Wave cannot cope with totalling along a 
;	dimension.
;
; CATEGORY:
;	Optimal Detection Package, v3.0.0
;
; CALLING SEQUENCE:
;	Result = TOTAL_1D( A [, Dim] )
;
; INPUTS:
;	A:  An array of type floating point.
;
; OPTIONAL INPUTS:
;	Dim:  The dimension index over which to total, starting with "1" (not 
;		0 as IDL usually uses).  If not set then the total of the 
;		entire array is calculated.
;
; KEYWORD PARAMETERS:
;	-
;
; OUTPUTS:
;	Result:  Returns the total of all elements in A if Dim is not set.  If 
;		Dim is set, then it returns an array of one dimension smaller 
;		than A comprising totals over the omitted dimension.
;
; USES:
;	-
;
; PROCEDURE:
;	This function totals over one dimension in the array, returning an 
;	array one dimension smaller than the input.
;
; EXAMPLE:
;	Input:
;	  a = findgen( 5, 6, 7 )
;	  result = total_1d( a, 2 )
;	  help, result
;	This gives:
;	  RESULT          FLOAT     = Array[5, 7]
;
; MODIFICATION HISTORY:
;	Written by:	Myles R. Allen (m.r.allen@rl.ac.uk), 1999-05-15 (v1.0)
;	Modified:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2004-06-28 
;			(Documentation for inclusion in routine library)
;	Modified:	DAS, 2005-09-01 (Updated documentation;  v3.0 of 
;			Optimal Detection Package)
;-

function total_1d,A,dim

; Copyright (1999) Myles Allen, Space Science Department, Rutherford Appleton Laboratory
; Prepared under contract to the Hadley Centre for Climate Prediction and Research
;+
; Name: function total_1d
;
; Description:
; Manual implementation of the IDL total function to get around the fact
; that PV-wave can't cope with totalling along a dimension
;
; Method:
; Uses the rebin and reform functions instead to perform the same operation
;
; History:
; Vers.	Date		Comment			Author
; ----  -------- 	--------		--------
; 1.0   15/05/99 	Original code 	Myles Allen m.r.allen@rl.ac.uk
;
; Code Description: IDL / PV-WAVE
;
; Category: 		Function
;
; Classification keywords: general maths
;
; Calling sequence: b=total(A,dim)
;
; Example call:
;
; Inputs:
; 		arg1:		A = real array
;
; Optional Inputs:
;       arg2:		dim = dimension to total over
;
; Keywords:			None
;
; Outputs:			Function value only
;
; Optional Outputs: None
; Return Value:     Either total of all elements in A or
; 					array of dimensions one smaller than A
; 					comprising totals over the omitted dimension
; Common Blocks: 	None
; Side Effects: 	None known
; Restrictions: 	Not tested for complex matrices
;-
;;; Just ignore the next four lines
author_name = '$Author: m.r.allen@rl.ac.uk $'
date_name = '$Date: 1999/05/15 10:30 $'
version_name = '$Revision: 1.0 $'

sa=size(A)
if (sa(0) eq 0) then return,A
if (n_params() lt 2) then return,total(A)
if (dim gt sa(0)) then stop,'total1d: No. of dimensions must be ge',dim
n=total(sa(dim))
sa(dim)=1
if (sa(0) eq 1) then return,n*reform(rebin(a,sa(1)))
if (sa(0) eq 2) then return,n*reform(rebin(a,sa(1),sa(2)))
if (sa(0) eq 3) then return,n*reform(rebin(a,sa(1),sa(2),sa(3)))
if (sa(0) eq 4) then return,n*reform(rebin(a,sa(1),sa(2),sa(3),sa(4)))
if (sa(0) eq 5) then return,n*reform(rebin(a,sa(1),sa(2),sa(3),sa(4),sa(5)))
if (sa(0) eq 6) then return,n*reform(rebin(a,sa(1),sa(2),sa(3),sa(4),sa(5),sa(6)))
if (sa(0) eq 7) then return,n*reform(rebin(a,sa(1),sa(2),sa(3),sa(4),sa(5),sa(6),sa(7)))
if (sa(0) eq 8) then return,n*reform(rebin(a,sa(1),sa(2),sa(3),sa(4),sa(5),sa(6),sa(7),sa(8)))
stop,'total_1d function failed'
end




