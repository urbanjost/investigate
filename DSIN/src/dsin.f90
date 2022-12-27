double precision function dsin (x)
! August 1980 edition.  W. Fullerton, Los Alamos Scientific Lab.
!
! this routine is based on the algorithm of Cody and Waite in
! Argonne tm-321, software manual working note number 1
!
double precision x, sincs(15), pihi, pilo, pirec, pi2rec, xsml, &
 & xwarn, xmax, y, xn, sgn, f, dint, dcsevl, d1mach, dsqrt

external d1mach, dcsevl, dint, dsqrt, initds
!
! series for sin    on the interval  0.00000e+00 to  2.46740e+00
!                                        with weighted error   2.56e-34
!                                         log weighted error  33.59
!                               significant figures required  33.01
!                                    decimal places required  34.18
!
      data sincs(  1) / -0.374991154955873175839919279977323464d0/
      data sincs(  2) / -0.181603155237250201863830316158004754d0/
      data sincs(  3) /  0.005804709274598633559427341722857921d0/
      data sincs(  4) / -0.000086954311779340757113212316353178d0/
      data sincs(  5) /  0.000000754370148088851481006839927030d0/
      data sincs(  6) / -0.000000004267129665055961107126829906d0/
      data sincs(  7) /  0.000000000016980422945488168181824792d0/
      data sincs(  8) / -0.000000000000050120578889961870929524d0/
      data sincs(  9) /  0.000000000000000114101026680010675628d0/
      data sincs( 10) / -0.000000000000000000206437504424783134d0/
      data sincs( 11) /  0.000000000000000000000303969595918706d0/
      data sincs( 12) / -0.00000000000000000000000037135734157d0/
      data sincs( 13) /  0.000000000000000000000000000382486123d0/
      data sincs( 14) / -0.000000000000000000000000000000336623d0/
      data sincs( 15) /  0.000000000000000000000000000000000256d0/
!
! pihi + pilo = pi.  pihi is exactly representable on all machines
! with at least 8 bits of precision.  whether it is exactly
! represented depends on the compiler.  this routine is more
! accurate if it is exactly represented.
      data pihi /   3.140625d0 /
      data pilo /   9.6765358979323846264338327950288d-4/
      data pirec /  0.31830988618379067153776752674503d0 /
      data pi2rec / 0.63661977236758134307553505349006d0 /
      data ntsn, xsml, xwarn, xmax / 0, 3*0.0d0 /
!
      if (ntsn.ne.0) go to 10
      ntsn = initds (sincs, 15, 0.1*sngl(d1mach(3)))
!
      xsml = dsqrt (2.0d0*d1mach(3))
      xmax = 1.0d0/d1mach(4)
      xwarn = dsqrt (xmax)
!
 10   continue
      y = dabs (x)
      if (y.gt.xmax) call seteru ( &
     &  'dsin    no precision because abs(x) is big', 42, 2, 2)
      if (y.gt.xwarn) call seteru ( &
     &  'dsin    answer lt half precision because abs(x) is big', &
     &  54, 1, 1)
!
      dsin = x
      if (y.lt.xsml) return
!
      xn = dint (y*pirec+0.5d0)
      n2 = dmod (xn, 2.0d0) + 0.5d0
      sgn = x
      if (n2.ne.0) sgn = -sgn
      f = (y-xn*pihi) - xn*pilo
!
      dsin = f + f*dcsevl(2.0d0*(f*pi2rec)**2-1.0d0, sincs, ntsn)
      if (sgn.lt.0.0d0) dsin = -dsin
      if (dabs(dsin).gt.1.0d0) dsin = dsign (1.0d0, dsin)
!
end function dsin
