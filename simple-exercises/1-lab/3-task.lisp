(defun determineSet(A B C)(union (intersection A
												 (intersection B
															   (set-difference (union A B)
																			   C)))
								   (union (intersection A
														(set-difference (union A C)
																		B))
										  (union C
												 (set-difference (union B C)
																 A)))
								   )
  )

(write (determineSet '(Z X C S A D F) '((R) (30) (3) 23) '(U I 8 9 6 5 4 3 (1 2 3))))
