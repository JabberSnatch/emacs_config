(setq-default c-echo-syntactic-information-p t)
(c-add-style "yolosquad"
	     '((c-basic-offset . 4)
	       (tab-width . 4)
	       (c-offsets-alist
		(substatement-open . 0)
		))))

(setq-default c-default-style
	      '((java-mode . "java")
		(awk-mode . "awk")
		(c-mode . "yolosquad")
		(c++-mode . "yolosquad")
		(other . "gnu")))
