Function Sign, x

;+
; NAME:
;	SIGN
; VERSION:
;	3.0
; PURPOSE:
;	Gives the sign of X, i.e. 1 for positive, -1 for negative, 0 for 0.
; CATEGORY:
;	Mathematical Function (General).
; CALLING SEQUENCE:
;	Result = SIGN(X)
; INPUTS:
;    X
;	Numerical, otherwise arbitrary.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	Returns the value of SIGN(X), see above, as an long integer.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	For complex X the result is SIGN(REAL(X)), the imaginary part is ignored
; PROCEDURE:
;	Straightforward.  Using CAST from MIDL.
; MODIFICATION HISTORY:
;	Created 15-JUL-1991 by Mati Meron.
;	Modified 25-DEC-1991 by Mati Meron.
;	Modified 5-DEC-1993 by Mati Meron.  Output type changed to LONG.
;-

;    temx = Cast(x,0,5)
	temx=x
    return, long((temx gt 0)) - (temx lt 0)
end
