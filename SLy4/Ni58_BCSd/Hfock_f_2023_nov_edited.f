
      common/en1/en1(40)
      common/rf/rf(800,40)/rf1/rf1(800,40),rf2(800,40)
      dimension uv(40)
      common/al/al(40)/aj/aj(40)/en/en(40)/ki/k
      common/param/h1,h2,ht,rt,n,n0,n1
      common/lwf/l(3),alo(40),ln
      common/index/ind/coef/alfa(40),beta(3)
      common/iprint/iter,irf,irf1/art/ar
      common/coeff/alf1(2,40),bet1(2,3)
      dimension bet2(2,3),alf2(2,40),uvl(40),enk(40)
      common/rr/rr1(800,6,3),rr2(800,6,3)
      dimension rp(6,4)
      common/yy1/y1(2,40)/y/y2(2,40)/coef1/alf2,bet2
      common/ass/pmn(2),pml,a
	common/npri/npri(40)
	common/npri1/npri1(40)
        common/parname/paramet,hypparamet
!      COMMON/EPSUV/EPS(100),U(100),V(100)                 !
      dimension rj(100)                                    ! BCS
      double precision, dimension(40) :: aal,aaj,een,eps,uu,vv
      double precision :: RLAMP, RLAMN, CP, CN, GP, GN, RLAMP1, RLAMN1
!      real :: aal(40),aaj(40),een(40)                     ! BCS
!      COMMON/spsv/aal(40),aaj(40),een(40)                 ! BCS

      real :: nameZ, nameA
      character(len = 40) :: file_name
      character(len = 20) :: paramet, hypparamet
      character(len = 3) :: chel
      integer :: flag1, oddl

!      paramet = "_SIII.1"
      nameZ=0.
      nameA=0.
      do i=1, l(1)
        nameZ=nameZ+alo(i)
      end do
      do i=1, ln
        nameA=nameA+alo(i)
      end do

      if ((int(nameZ).gt.2).and.(int(nameZ).lt.21).or.
     *(int(nameZ).eq.28).or.(int(nameZ).eq.40)) then
      select case (int(nameZ))
        case(3)
        if (pml.eq.0.) then
        write(file_name,"(A2,I0)")"Li",int(nameA)
        else
        write(file_name,"(A2,I0,A1)")"Li",int(nameA),"L"
        end if
        case(4)
        if (pml.eq.0.) then
        write(file_name,"(A2,I0)")"Be",int(nameA)
        else
        write(file_name,"(A2,I0,A1)")"Be",int(nameA),"L"
        end if
        case(5)
        if (pml.eq.0.) then
        write(file_name,"(A1,I0)")"B",int(nameA)
        else
        write(file_name,"(A1,I0,A1)")"B",int(nameA),"L"
        end if
        case(6)
        if (pml.eq.0.) then
        write(file_name,"(A1,I0)")"C",int(nameA)
        else
        write(file_name,"(A1,I0,A1)")"C",int(nameA),"L"
        end if
        case(7)
        if (pml.eq.0.) then
        write(file_name,"(A1,I0)")"N",int(nameA)
        else
        write(file_name,"(A1,I0,A1)")"N",int(nameA),"L"
        end if
        case(8)
        if (pml.eq.0.) then
        write(file_name,"(A1,I0)")"O",int(nameA)
        else
        write(file_name,"(A1,I0,A1)")"O",int(nameA),"L"
        end if
        case(9)
        if (pml.eq.0.) then
        write(file_name,"(A1,I0)")"F",int(nameA)
        else
        write(file_name,"(A1,I0,A1)")"F",int(nameA),"L"
        end if
        case(10)
        if (pml.eq.0.) then
        write(file_name,"(A2,I0)")"Ne",int(nameA)
        else
        write(file_name,"(A2,I0,A1)")"Ne",int(nameA),"L"
        end if
        case(11)
        if (pml.eq.0.) then
        write(file_name,"(A2,I0)")"Na",int(nameA)
        else
        write(file_name,"(A2,I0,A1)")"Na",int(nameA),"L"
        end if
        case(12)
        if (pml.eq.0.) then
        write(file_name,"(A2,I0)")"Mg",int(nameA)
        else
        write(file_name,"(A2,I0,A1)")"Mg",int(nameA),"L"
        end if
        case(13)
        if (pml.eq.0.) then
        write(file_name,"(A2,I0)")"Al",int(nameA)
        else
        write(file_name,"(A2,I0,A1)")"Al",int(nameA),"L"
        end if
        case(14)
        if (pml.eq.0.) then
        write(file_name,"(A2,I0)")"Si",int(nameA)
        else
        write(file_name,"(A2,I0,A1)")"Si",int(nameA),"L"
        end if
        case(15)
        if (pml.eq.0.) then
        write(file_name,"(A1,I0)")"P",int(nameA)
        else
        write(file_name,"(A1,I0,A1)")"P",int(nameA),"L"
        end if
        case(16)
        if (pml.eq.0.) then
        write(file_name,"(A1,I0)")"S",int(nameA)
        else
        write(file_name,"(A1,I0,A1)")"S",int(nameA),"L"
        end if
        case(17)
        if (pml.eq.0.) then
        write(file_name,"(A2,I0)")"Cl",int(nameA)
        else
        write(file_name,"(A2,I0,A1)")"Cl",int(nameA),"L"
        end if
        case(18)
        if (pml.eq.0.) then
        write(file_name,"(A2,I0)")"Ar",int(nameA)
        else
        write(file_name,"(A2,I0,A1)")"Ar",int(nameA),"L"
        end if
        case(19)
        if (pml.eq.0.) then
        write(file_name,"(A1,I0)")"K",int(nameA)
        else
        write(file_name,"(A1,I0,A1)")"K",int(nameA),"L"
        end if
        case(20)
        if (pml.eq.0.) then
        write(file_name,"(A2,I0)")"Ca",int(nameA)
        else
        write(file_name,"(A2,I0,A1)")"Ca",int(nameA),"L"
        end if
        case(28)
        if (pml.eq.0.) then
        write(file_name,"(A2,I0)")"Ni",int(nameA)
        else
        write(file_name,"(A2,I0,A1)")"Ni",int(nameA),"L"
        end if
        case(40)
        if (pml.eq.0.) then
        write(file_name,"(A2,I0)")"Zr",int(nameA)
        else
        write(file_name,"(A2,I0,A1)")"Zr",int(nameA),"L"
        end if
      end select
      file_name=trim(file_name)//trim(adjustl(paramet))//
     *trim(adjustl(hypparamet))
      open(1,file=trim(adjustl(file_name))//".1",status='unknown')
      open(2,file=trim(adjustl(file_name))//".2",status='unknown')
      open(3,file=trim(adjustl(file_name))//".3",status='unknown')
      open(4,file=trim(adjustl(file_name))//".4",status='unknown')
      end if

      if (int(nameA)/2*2.eq.int(nameA)) then
      flag1=1
      else
      if (int(nameZ)/2*2.eq.int(nameZ)) then
      flag1=2
      do i=1+l(2),l(1)+l(2)
      if (int(alo(i))/2*2.ne.int(alo(i))) then
      oddl=i
      end if
      end do
      else
      flag1=3
      do i=1,l(1)
      if (int(alo(i))/2*2.ne.int(alo(i))) then
      oddl=i
      end if
      end do
      end if
      end if

      do 300 iBCS = 1, 5
      if (iBCS.gt.1) then
      if (CP.gt.0.) then
      do jBCS=1,l(1)
      alo(jBCS)=(2*aj(jBCS)+1)*(REAL(VV(jBCS)))**2
      end do
      end if
      if (CN.gt.0.) then
      do jBCS=l(1)+1,l(1)+l(2)
      alo(jBCS)=(2*aj(jBCS)+1)*(REAL(VV(jBCS)))**2
      end do
      end if
      end if

      if (iBCS.eq.1) then
      call put1(epsil)
      else
      do iii=1,40
      en(iii)=en(iii)*5.06769e-3
      end do
      end if

      k=0
      ke=100
	na=n+1
	nb=n+2
      do 228 j2=1,40
      enk(j2)=en(j2)
  228 continue
      r=h1
      nk=n0
      kj=2
      h=h1
  221 continue
      do 222 i=kj,nk
      call prot(i,r,rp)
      do 223 j1=1,3
      do 223 j2=1,6
      rr1(I,j2,j1)=rp(j2,j1)
      rr2(i,j2,j1)=rp(j2,j1)
  223 continue
  222 r=r+h
      if(kj.gt.2)goto 205
      r=r-h+h2
      nk=n
      kj=n0+1
      h=h2
      goto 221
  205 if(kj.eq.na)goto 20
      r=ht
      nk=nb
      kj=na
      h=ht
      goto 221


   20 if(iter.ne.1)goto 21
      write(*,101)k,amax,amax1,ke
      write(*,102)(npri1(i),al(i),aj(i),en(i),i=1,ln)
      if(irf.ne.1)goto 21
      write(*,103)
      write(*,104)(npri1(i),al(i),aj(i),i=1,ln)
      write(*,105)((rf(i,j),j=1,ln),i=1,n)
      WRITE(*,105)((Rf1(i,j),j=1,ln),i=1,n)
   21 r=0.
      nt=rt/h1+1.1
      ind=1
      do 81 i=na,nb
      r=r+ht
      do 220 j1=1,6
      do 220 j2=1,3
      rp(j1,j2)=rr1(i,j1,j2)
  220 continue
      call ql(i,r,rp,uvl)
      jn=0
      do 84 jm=1,3
      l1=l(jm)
      if(l1.eq.0)goto 84
      ir=i-n
      do 82 j1=1,l1
      jn=jn+1
   82 alf1(ir,jn)=uvl(jn)
      bet1(ir,jm)=beta(jm)
   84 continue
   81 continue
      jl=0
      r=0.
      do 85 jm=1,3
      l1=l(jm)
      if(l1.eq.0)goto 85
      bet2(1,jm)=4./3./ht*bet1(1,jm)-bet1(2,jm)/6./ht
      bet2(2,jm)=(bet1(2,jm)-2.*bet1(1,jm))/ht**3
      do 86 jn=1,l1
      jl=jl+1
      alf2(1,jl)=(4.*alf1(1,jl)-alf1(2,jl))/3.
      alf2(2,jl)=2.*(alf1(2,jl)-alf1(1,jl))/3./ht**2
   86 continue
   85 continue
      call teyl
      do 25 j=1,ln
      y1(1,j)=y2(1,j)
      y1(2,j)=y2(2,j)
   25 continue
      r=rt
      do 288 j1=1,6
      do 288 j2=1,3
      rp(j1,j2)=rr1(nt,j1,j2)
  288 continue
      call ql(nt,rt,rp,uv)
      jn=0
      do 584 jm=1,3
      l1=l(jm)
      if(l1.eq.0)goto 584
      do 582 j1=1,l1
      jn=jn+1
  582 alf1(1,jn)=alfa(jn)
      bet1(1,jm)=beta(jm)
  584 continue
      do 1 i=nt,n1
      ii=i+1
      do 232 j1=1,6
      do 232 j2=1,3
      rp(j1,j2)=rr1(ii,j1,j2)
  232 continue
      rq=r+h1
      call ql(ii,rq,rp,uv)
      jn=0
      do 684 jm=1,3
      l1=l(jm)
      if(l1.eq.0)goto 684
      do 682 j1=1,l1
      jn=jn+1
  682 alf1(2,jn)=alfa(jn)
      bet1(2,jm)=beta(jm)
  684 continue
      call solv(i,r,h1)
      jn=0
      do 784 jm=1,3
      l1=l(jm)
      if(l1.eq.0)goto 784
      do 782 j1=1,l1
      jn=jn+1
  782 alf1(1,jn)=alf1(2,jn)
      bet1(1,jm)=bet1(2,jm)
  784 continue
    1 r=r+h1
      n7=n-n1
      r1=-((n0-1)*h1)-h2*(n-n0)
      ind=2
      do 26 j=1,ln
      y1(1,j)=rf(n,j)
cccccccccccccccccccccccccccccccccc
c	if (al(j).ne.0) goto 26
c      if(ke.ne.0)y1(1,j)=rf(n,j)*1.e-4
ccccccccccccccccccccccccccccccccccccccccc   15.1.95
  26  y1(2,j)=rf1(n,j)
      rq=-r1
      do 272 j1=1,6
      do 272 j2=1,3
      rp(j1,j2)=rr1(n,j1,j2)
  272 continue
      call ql(n,rq,rp,uvl)
      jn=0
      do 884 jm=1,3
      l1=l(jm)
      if(l1.eq.0)goto 884
      do 882 j1=1,l1
      jn=jn+1
  882 alf1(1,jn)=alfa(jn)
      bet1(1,jm)=beta(jm)
  884 continue
      h=h2
      do 2 ik=1,n7
      r=-r1
      i=n-ik+1
      if(i.le.n0)h=h1
      iq=i-1
      do 276 j1=1,6
      do 276 j2=1,3
      rp(j1,j2)=rr1(iq,j1,j2)
  276 continue
      rq=r-h
      call ql(iq,rq,rp,uv)
      jn=0
      do 984 jm=1,3
      l1=l(jm)
      if(l1.eq.0)go to 984
      do 982 j1=1,l1
      jn=jn+1
  982 alf1(2,jn)=alfa(jn)
      bet1(2,jm)=beta(jm)
  984 continue
      call solv(i,r1,h)
      jn=0
      do 994 jm=1,3
      l1=l(jm)
      if(l1.eq.0)goto 994
      do 992 j1=1,l1
      jn=jn+1
  992 alf1(1,jn)=alf1(2,jn)
      bet1(1,jm)=bet1(2,jm)
  994 continue
    2 r1=r1+h
      nq=n1+1
      rq=-r1-h1
      do 296 j1=1,6
      do 296 j2=1,3
      rp(j1,j2)=rr1(nq,j1,j2)
  296 continue
      call ql(nq,rq,rp,uv)
      jn=0
      do 538 jm=1,3
      l1=l(jm)
      if(l1.eq.0)goto 538
      do 537 j1=1,l1
      jn=jn+1
  537 alf1(1,jn)=alfa(jn)
      bet1(1,jm)=beta(jm)
  538 continue
      call solv2
ccccccccccc
	do 777 kkk=1,ln
	nprin=1
	nnnn=n-1
	do 778 kkkk=2,nnnn
	if(rf(kkkk,kkk)*rf(kkkk+1,kkk).lt.0.)nprin=nprin+1
778	continue
	npri1(kkk)=nprin
777	continue
cccccccccccccccccc
      amax=abs(en1(1))
      jk=1
      do 15 j=2,ln
cccccccc
c      if(j.eq.ln-1)goto 15
cccccccc
      if(amax.gt.abs(en1(j)))goto 15
      amax=abs(en1(j))
      jk=j
   15 continue
      do 16 in=1,ln
ccccccccccccccc
	if(npri(in).eq.npri1(in))goto 444
	if(npri(in).gt.npri1(in))goto 445
	en(in)=1.2*en(in)
	goto 446
445	en(in)=.9*en(in)
	goto 446
444	continue
ccccccccccccccccccc
c      if((en(in)+en1(in)).lt.0.)en(in)=en(in)+en1(in)
       en(in)=en(in)+en1(in)
446	continue
      en1(in)=0.
   16 continue
      k=k+1
      amax=amax/en(jk)
      if(abs(amax).gt.epsil)goto 20
      ama=abs(enk(1)-en(1))
      jjk=1
      do 217 jc=2,ln
      if(ama.gt.abs(enk(jc)-en(jc)))goto 217
      ama=abs(enk(jc)-en(jc))
      jjk=jc
  217 continue
      amax1=ama/en(jjk)
      if(abs(amax1).lt.epsil)goto 89
      ke=k
      do 227 j2=1,40
      enk(j2)=en(j2)
  227 continue
      r=h1
      nk=n0
      h=h1
      kj=2
  225 continue
      do 230 i=kj,nk
      call prot(i,r,rp)
      do 231 j1=1,3
      do 231 j2=1,6
      rr1(i,j2,j1)=ar*rp(j2,j1)+(1.-ar)*rr1(i,j2,j1)
ccccccc26.11.94      rr2(i,j2,j1)=rp(j2,j1)
  231 continue
  230 r=r+h
      if(kj.gt.2)goto 207
      r=r-h+h2
      nk=n
      kj=n0+1
      h=h2
      goto 225
  207 if(kj.eq.na)goto 20
      r=ht
      nk=nb
      kj=na
      h=ht
      go to 225
   89 continue
      if (iBCS.eq.1) then
      CP=0.d0
      CN=0.d0
      end if

      GP=0.58976     !GNP, 0.840 \ GP_0 = 17/A (MeV)
      GN=0.39717  !0.686 \ GN_0 = 23/A (MeV)

      call result(CP,GP,CN,GN)
      do 29 im=1,ln
      en(im)=en(im)/5.06769e-3
   29 continue
      call output(irf1)
  101 format(2x,'iter. number',i3,2x,'epsil='2e13.5,i5)
  102 format(4x,'N=',i2,3x,'L=',f3.1,4x,'J=',f3.1,2x,f10.5)
  103 format('    RADIAL FUNCTIONS')
  104 format(6x,7('N',f3.1,'L',f3.1,1x,'J',f3.1,3x))
  105 format(4x,7e10.4)

C     BCS starts here, except see GN, GP above 'call result'

      IGRN=0
      IGRP=0

      if (iBCS.eq.1) then
      DO 501 I=1,l(1)
      IGRP=IGRP+2*aj(I)+1                               ! last occupied
      IF(IGRP.LT.nameZ) GO TO 501                         ! proton level
      IGRSTP=I
      RLAMP = en(I)
      RLAMP1 = en(I)+0.001D0
!      print*, RLAMP, RLAMP1, 11
      GO TO 502
  501 CONTINUE
  502 CONTINUE
!      RLAMP=en(4)

!      print*, I, RLAMP; pause

      DO 511 I=1+l(1),l(1)+l(2)
      IGRN=IGRN+2*aj(I)+1                            ! last occupied
      IF(IGRN.LT.(nameA-nameZ)) GO TO 511                  ! neutron level
      IGRSTN=I
      RLAMN = en(I)
      RLAMN1 = en(I)+0.001D0
!      print*, RLAMN, RLAMN1, 12; pause
      GO TO 512
  511 CONTINUE
  512 CONTINUE
!      RLAMN=en(10)
      end if

!      print*, RLAMP, RLAMN; pause

!  144 IF(IGRSTN.NE.0.AND.IGRSTP.NE.0) GO TO 146
!      WRITE(*, 6028)IGRN, IGRP
!      stop
!  146 CONTINUE

      do i=1,40
      aaj(i)=aj(i)
      aal(i)=al(i)
      een(i)=en(i)
      end do
!      do i=1,l(1)+l(2)
!      print*, al(i), aj(i), en(i), aal(i), aaj(i), een(i)
!      end do
!      pause

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>  BEGIN BCS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

!      print*, flag1,oddl; pause

      select case(flag1)
        case(1)
!       gap and lambda for protons
        CALL CLAM(1,l(1), int(nameZ)+0.d0,GP+0.d0,0,0,CP,
     *  RLAMP,real(EGRP,8),aal,aaj,een,RLAMP1)
!       gap and lambda for neutrons
        CALL CLAM(l(1)+1,l(1)+l(2),int(nameA-nameZ)+0.D0,GN+0.d0,0,0,CN,
     *  RLAMN,real(EGRN,8),aal,aaj,een,RLAMN1)
        case(2)
!       gap and lambda for protons
        CALL CLAM(1,l(1), int(nameZ)+0.d0,GP+0.d0,0,0,CP,
     *  RLAMP,real(EGRP,8),aal,aaj,een,RLAMP1)
!       gap and lambda for neutrons, odd case
        CALL CLAMODD(l(1)+1,l(1)+l(2),int(nameA-nameZ)+0.D0,GN+0.d0,0,0,
     *  CN,RLAMN,real(EGRN,8),aal,aaj,een,RLAMN1,oddl)
        case(3)
!       gap and lambda for protons, odd case
        CALL CLAMODD(1,l(1), int(nameZ)+0.d0,GP+0.d0,0,0,CP,
     *  RLAMP,real(EGRP,8),aal,aaj,een,RLAMP1,oddl)
!       gap and lambda for neutrons
        CALL CLAM(l(1)+1,l(1)+l(2),int(nameA-nameZ)+0.D0,GN+0.d0,0,0,CN,
     *  RLAMN,real(EGRN,8),aal,aaj,een,RLAMN1)
      end select
!      print*, CP, CN; pause
!     eps, u, v
      CALL EPSUVS(CP,CN,RLAMP,RLAMN,l(1),l(2),aal,aaj,een,eps,UU,VV)
!      do i=1,l(1)+l(2)
!      print*, aal(i), aaj(i), een(i), eps(i), VV(i), UU(i)
!      end do
!      pause

c     ***************** start printing BCS results *************************************
      write(*,'(1x)')
	write(*,'(A4,I3,A7,I3)')' A =',int(nameA),'    Z =',int(nameZ)
	write(*,'(1x)')
      write(*,'(A11,F6.3,A4,A11,F6.3,A4)')'      G_p =',GP,    ' MeV',
     *                                    '      G_n =',GN,    ' MeV'
      write(*,'(1x)')
	write(*,'(A8,F5.2,A4,A12,F7.3,A4)')'   C_p =',CP,    ' MeV',
     *                               '  Lambda_p =',rlamp, ' MeV'
	write(*,'(A8,F5.2,A4,A12,F7.3,A4)')'   C_n =',CN,    ' MeV',
     *                               '  Lambda_n =',rlamn, ' MeV'
      write(*,'(1x)')

      CP1=0.
      CN1=0.
      do iii=1,l(1)
      CP1=CP1+(2*aaj(iii)+1)*UU(iii)*VV(iii)
      end do
      do iii=1+l(1),l(1)+l(2)
      CN1=CN1+(2*aaj(iii)+1)*UU(iii)*VV(iii)
      end do
      select case (flag1)
      case (1)
      CP1=CP1*GP/2
      CN1=CN1*GN/2
      case (2)
      CP1=CP1*GP/2
      CN1=GN/2*(CN1-2*UU(oddl)*VV(oddl))
      case (3)
      CP1=GP/2*(CP1-2*UU(oddl)*VV(oddl))
      CN1=CN1*GN/2
      end select
      write(*,'(A16,F5.2,A8,F5.2)')'Checking: C_p1 =',CP1,'  C_n1 =',CN1

c      pause

      select case (iBCS)
         case(1)
         open(16,file='bcs1.out',status='unknown')
         case(2)
         open(16,file='bcs2.out',status='unknown')
         case(3)
         open(16,file='bcs3.out',status='unknown')
         case(4)
         open(16,file='bcs4.out',status='unknown')
         case(5)
         open(16,file='bcs5.out',status='unknown')
      end select

!      OPEN(16,FILE='bcs.out',FORM='FORMATTED',STATUS='unknown')
 6017 FORMAT(/20X,'EVEN-EVEN NUCLEUS Z =',I3,'  A =',I3
     */1X,'GN =',F10.3
     *,5X,'GP =',F10.3/1X,'CN =',F10.5,5X,'CP =',F10.5,5X,'LAMN =',
     *F10.5,5X,'LAMP =',F10.5)
 6019 FORMAT(/9X,'I',' INDE',9X,'E',7X,'EPS',9X,'U',9X,'V')
 6020 FORMAT(I10,I3,6F10.5)

      WRITE(16, 6017)int(nameZ),int(nameA),GN,GP,CN,CP,RLAMN,RLAMP

      write(16,'(1x)')
      Zp=0.d0
!      IF (CP.eq.0.D0) THEN
!      RLAMP = RLAMP1
!      END IF
      WRITE(16,'(A23)')'          P R O T O N S'
      DO 3 I=1,l(1)
      IF (CP.gt.0.) THEN
      RJ(i)=aaj(i)+0.5
	Zp=Zp+RJ(i)*(1-(een(I)-RLAMP)/EPS(I))
      ELSE
      Zp = Zp + alo(I)
      END IF
!      print*, 21, I, en(I), RLAMP, en(I)-RLAMP, 1-(en(I)-RLAMP)/EPS(I)
!      pause
    3 WRITE(16, 6020)I,npri1(I),aal(I),aaj(I),een(I),EPS(I),UU(I),VV(I)
      print*,'Np=',Zp



      write(16,'(1x)')
      WRITE(16, '(A25)')'          N E U T R O N S'
      Zn=0.d0
!      IF (CN.eq.0.D0) THEN
!      RLAMN = RLAMN1
!      END IF
      DO 4 I=l(1)+1,l(1)+l(2)
      IF (CN.gt.0.) THEN
      RJ(I)=aaj(i)+0.5
	Zn=Zn+RJ(I)*(1-(en(I)-RLAMN)/EPS(I))
      ELSE
      Zn = Zn + alo(I)
      END IF
!      print*, 22, I, en(I), RLAMN, en(I)-RLAMN, 1-(en(I)-RLAMN)/EPS(I)
!      pause
    4 WRITE(16, 6020)I,npri1(I),al(I),aj(I),en(I),EPS(I),UU(I),VV(I)
      print*,'Nn=',Zn

      if (CP.gt.0.) then
      do jBCS=1,l(1)
      alo(jBCS)=(2*aj(jBCS)+1)*(REAL(VV(jBCS)))**2
      end do
      end if
      if (CN.gt.0.) then
      do jBCS=l(1)+1,l(1)+l(2)
      alo(jBCS)=(2*aj(jBCS)+1)*(REAL(VV(jBCS)))**2
      end do
      end if

!      do iii=1,40
!      print*, alo(iii)
!      end do
!      pause

      close(16)
  300 continue

 6021 FORMAT(A5,F10.5,A7,F10.5,A9,F10.5)
      WRITE(1, 6021)'GP = ', GP,', CP = ', CP, ', LAMP = ', RLAMP
      WRITE(1, 6021)'GN = ', GN,', CN = ', CN, ', LAMN = ', RLAMN
      if ((int(nameZ).ge.2).and.(int(nameZ).le.40)) then
      close(1)
c      close(2)
c      close(3)
c      close(4)
      end if

      stop
      end
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine put1(epsil)
      common/sl/const(2),cons,const2
      common/tsk/t(16),tl(15)
      common/ass/pmn(2),pml,a
      common/bf/bf(40)
      common/rf/rf(800,40)/rf1/rf1(800,40),rf2(800,40)
      common/al/al(40)/aj/aj(40)/en/en(40)
      common/param/h1,h2,ht,rt,n,n0,n1
      common/lwf/l(3),alo(40),ln
      common/iprint/iter,irf,irf1/art/ar
c         for light hypernuclei
      r0=8.1
      r1=3.6
c             for heavy
c      r0=5.6
c      r1=3.5
c               208Pb
c      r0=7.2
c      r1=4.8
c         for medium nuclei (Zr)
c      r0=6.0
c      r1=3.0
      write (*,197)
	write(1,197)
  197 format(9x,'t0',13x,'t1',13x,'t2',13x,'t3',13x,'x0',13x,'x1')
      write(*,196)(t(i),i=1,6)
	write(1,196)(t(i),i=1,6)
      write(*,196)(tl(i),i=1,6)
	write(1,196)(tl(i),i=1,6)
      write(*,198)
	write(1,198)
  198 format(9x,'x2',13x,'x3',13x,'gg',13x,'W0',13x,'W1',13x,'aa')
      write(*,196)(t(i),i=7,12)
	write(1,196)(t(i),i=7,12)
      write(*,196)(tl(i),i=7,12)
	write(1,196)(tl(i),i=7,12)
  196 format (4x,6e15.5)
      write(*,199)h1,h2,n
	write(1,199)h1,h2,n
	write(*,194)r0,r1
	write(1,194)r0,r1
	write(*,195)ht,rt
	write(1,195)ht,rt
  199 format (3x,'h1=',f7.4,2x,'h2=',f7.4,2x,'N=',i3)
  194 format (3x,'ro=',f7.4,2x,'r1=',f7.4)
  195 format (3x,'ht=',f7.4,2x,'rt=',f7.4)
cccccccccccccccccccccc   standard
c      ar=.4
      epsil=1.e-5
CCCCCCCCCCCCCCCCCCCCCC TEST Pb (Lanskoy)
       ar=.25
c       epsil=2.e-4
cccccccccccccccccccccccccccccccccccccccc
      bn=0.
      Write(*,720)ar,epsil
	write(1,720)ar,epsil
  720 format(2x,'ar=',e10.2,2x,'epsil=',e10.2)
      n1=r1/h1+1.1
      n0=r0/h1+1.1
      if(n0/2*2.eq.n0)n0=n0+1
      do 10 i=1,ln
      bn=bn+alo(i)
      en(i)=en(i)*5.06769e-3
   10 continue
      do 12 i=1,2
      pmn(i)=pmn(i)*5.06769e-3
   12 continue
      pml=pml*alo(ln)*5.06769e-3
      cf=0.
      do 44 j=1,2
      l0=0
	if(j.eq.2)l0=l(1)
	ll=1+l0
	l1=l(j)+l0
      ff=0.
      do 728 jj=ll,l1
      ff=ff+alo(jj)
  728 continue
      cf=cf+pmn(j)*ff
   44 continue
      cons=pml/(cf+pml)/bn
	aca=2.*cf/(pmn(1)+pmn(2))
	tca=(3.*aca/2.)**.33333
	fca=2./(tca+1/3./tca)
      do 47 j=1,2
      cONST(j)=.5*(cf+pml-pmn(j))/(cf+pml)/pmn(j)
   47 continue
      const2=0.
      if(pml.ne.0.)const2=.5*(pml/alo(ln)*(alo(ln)-1)+cf)/(cf+pml)/
     /pml*alo(ln)
      do 13 j=1,4
      t(j)=t(j)*5.06769e-3
      tl(j)=tl(j)*5.06769e-3
   13 continue
      t(6)=t(6)*5.06769e-3
      t(14)=t(14)*5.06769e-3
      t(15)=t(15)*5.06769e-3
      t(16)=t(16)*5.06769e-3
      tl(1)=tl(1)*(tl(5)/2.+1.)
      tl(4)=tl(4)*(tl(8)/2.+1.)
      if(tl(12).eq.0.)tl(4)=tl(4)*.375
      tl(10)=tl(10)*5.06769e-3
      tl(11)=tl(11)*5.06769e-3
      tl(13)=tl(13)*5.06769e-3
      tl(14)=tl(14)*5.06769e-3
      tl(15)=tl(15)*5.06769e-3
      call wavf
      return
      end
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine wavf
      common/bf/bf(40)/al/al(40)
      common/rf/rf(800,40)/rf1/rf1(800,40),rf2(800,40)
      common/param/h1,h2,ht,rt,n,n0,n1
      common/lwf/l(3),alo(40),ln
      common/r/ro(6,40)
      common/npri/npri(40)
	nb=n+2
	na=n+1
      do 367 ia=1,nb
      do 368 ip=1,40
      rf(ia,ip)=0.
      rf1(ia,ip)=0.
      rf2(ia,ip)=0.
  368 continue
  367 continue
      spi4=1.33134
      nst=0
      hnm=1.
      do 3 lll=1,10
      j=0
      ll=lll-1
      do 2 jj=1,3
      ljj=l(jj)
      if(ljj.eq.0)goto 2
      anm=1.1
      do 25 jjj=1,ljj
      j=j+1
      if(abs(al(j)-ll).gt..1)goto 25
      nm=anm
c      write(*,580)nm,ll
  580 format(5x,2i5)
      npri(j)=nm
      bb=bf(j)
      r=0.
      h=h1
      cn1=1.
      if(nm.le.2)goto 10
      nm1=nm-1
      do 20 nf=2,nm1
   20 cn1=cn1*nf
   10 cn2=1.
      nm2=2*nm+2*ll-1
      if(nm2.lt.2)goto 11
      do 21 nf=3,nm2,2
   21 cn2=cn2*nf
   11 c=sqrt(2.**(nm+ll+1)*cn1/cn2/bb)/spi4
      do 1 i=1,nb
      z=r/bb
      zl=1.
      z2=z**2
      if(z.ne.0..or.ll.ne.0)zl=z**ll
	if(-z2/2.+50.)7,8,8
7	e=1.e-20
	goto 9
8     e=exp(-z2/2.)
9	continue
      pl1=plag(nm-1,ll+.5,z2)
      pl2=plag(nm-2,ll+1.5,z2)
      pl3=plag(nm-3,ll+2.5,z2)
      rf(i,j)=c*zl*z*e*pl1
      rf1(i,j)=c*zl*e*((ll+1.-z2)*pl1-2.*z2*pl2)/bb
      rf2(i,j)=0.
      if(r.ne.0.)goto 12
      if(ll.ne.1)goto 13
      rf2(i,j)=2.*c/bb**2
      goto 13
   12 rf2(i,j)=c/bb**2*zl*z*e*((ll*(ll+1.)/z2-2.*ll-3.+z2)*pl1+
     +2.*(z2-3.-ll)*pl2+4.*z2*pl3)
   13 r=r+h
      if((r/h1+1.1).le.n0.or.h.ne.h1)goto 14
      r=r-h+h2
      h=h2
      goto 1
   14 if(i.ne.n)goto 1
      h=ht
      r=h
    1 continue
      if(ll.ge.2)goto 30
      if(ll.eq.0)goto 31
      ro(1,j)=rf2(1,j)
      goto 32
   31 ro(1,j)=rf1(1,j)
      goto 32
   30 cl1=1.
      do 33 nf=1,ll
   33 cl1=cl1*(nf+1.)
      cl2=1.
      nl2=2*ll+1
      do 34 nf=3,nl2,2
   34 cl2=cl2*nf
      ro(1,j)=sqrt(2.**(ll+3-nm)*cn2/cn1/bb)/spi4*cl1/cl2/bb**(ll+1)
   32 continue
c      write(*,566)ro(1,j)
  566 format(5x,e10.3)
      anm=anm+hnm
      nst=nst+1
   25 continue
    2 continue
c      write(*,555)ll,nst
c  555 format(5x,2i10)
      if(nst.eq.ln)return
      hnm=.5
    3 continue
      end
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      function plag(n1,a1,x1)
      n=n1
      a=a1
      x=x1
      l=1
   25 continue
      if(n.ge.0)goto 20
      plag=0.
      return
   20 pl=1.
      pl1=1.+a-x
      if(n-1)1,2,3
    1 plag=1.
      goto 38
    2 plag=pl1
      goto 38
    3 do 13 i=2,n
      p=pl1
      pl1=((2.*i+a-1.-x)*p-(i+a-1.)*pl)/i
   13 pl=p
      plag=pl1
   38 if(l.eq.1)return
      plag=plag*(-4)**n
      if(n.le.1)goto 21
      do 18 k=2,n
   18 plag=plag*k
   21 if(a.ge.0.)plag=plag*2.*x1
      return
      end
c
cccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine ql(i,r,rp,uv)
      external could
      common/sl/const(2),cons,const2
      common/v/uvh(40),uvb(40),upp(40)
      common/tsk/t(16),tl(15)/amm/amm(3)
      common/rf/rf(800,40)
      dimension uql(4),uv(40)
      dimension amb(2,2),aml(2),uq1(2),u3(2)
      common/al/al(40)/aj/aj(40)/en/en(40)/ki/k
      common/param/h1,h2,ht,rt,n,n0,n1
      common/lwf/l(3),alo(40),ln
      common/coef/alfa(40),beta(3)
      dimension ulw(4),uqw(2),amq(3)
      dimension rp(6,4)
      do 41 ij=1,6
   41 rp(ij,4)=rp(ij,1)+rp(ij,2)
      if(i.gt.3)goto 683
  683 hp=1.
      e=1./137.036
      const3=-e*(3./3.141593)**.3333
      vcce=const3*rp(1,1)**.3333
c      vcce=0.
      vc=could(r)+vcce
      d1=.25*(t(2)*(1.+.5*t(9))+t(3)*(1.+.5*t(10)))
      d2=.125*(3.*t(2)*(1.+2.*t(9))+t(3)*(1.+2.*t(10)))
      d3=.125*(t(3)*(1.+2.*t(10))-t(2)*(1.+2.*t(9)))
      d4=.125*(3.*t(2)*(1.+.5*t(9))-t(3)*(1.+.5*t(10)))
      d5=1.+.5*t(5)
      d6=t(5)+.5
      d7=1.+.5*t(8)
      d8=.5+t(8)
      d9=t(7)-1.
      dl1=.25*(tl(2)*(tl(6)/2.+1.)+tl(3)*(tl(7)/2.+1.))
      dl1c=.25*(tl(14)+tl(15))                      !CSB t1+t2
      dl2=tl(9)+1.
      dl3=tl(9)
      rl=1.
      rll=1.
      rm=1.
      dl0=tl(9)-1.
      rmm=1.
      if(dl2.ne.0.)rll=rp(1,4)**dl2
      if(dl3.ne.0.)rl=rp(1,4)**dl3
      if(dl0.ne.0..and.rp(1,3).ne.0.)rmm=rp(1,3)**dl0
      if(dl3.ne.0..and.rp(1,3).ne.0.)rm=rp(1,3)**dl3
      dl4=.125*(3.*tl(2)*(tl(6)/2.+1.)-tl(3)*(tl(7)/2.+1.))
      dl4c=.125*(3.*tl(14)-tl(15))                  !CSB 3t1-t2
      do 46 jn=1,2
      aml(jn)=dl1*rp(jn,4)
c=======================  CSB ========================
      aml(jn)=aml(jn)+0.25*(tl(14)+tl(15))*(rp(jn,1)-rp(jn,2))
c=====================================================
      uq1(jn)=-t(1)*d6*rp(1,jn)+d2*.5*rp(3,jn)+(d2+d3)*rp(2,jn)/
     /r+d3*rp(4,jn)-.5*t(16)*(rp(6,jn)+2./r*rp(5,jn))
Cnorm /r+d3*rp(4,jn)-.5*t(6)*(rp(6,jn)+2./r*rp(5,jn))
      u3(jn)=.5*tl(4)*rp(1,3)*(2.*rp(1,4)-rp(1,jn))
      if(tl(12).eq.0.)u3(jn)=tl(4)*(tl(9)+1.)*rp(1,3)*rl
      if(tl(12).eq.1.)u3(jn)=tl(4)*(rp(1,4)-.5*rp(1,jn))*rm
      jk=jn+2
      aa=1.
      if(jk.eq.3)aa=-1.
c     (1.20) in phd
      uql(jk)=tl(1)*rp(1,jk)+dl1*rp(4,jk)-dl4*rp(3,jk)-(2.*dl4-
     -dl1)*rp(2,jk)/r-.5*tl(10)*(rp(6,jk)+2./r*rp(5,jk))-
     -.5*aa*tl(11)*(rp(6,jk)+2./r*rp(5,jk))
c=======================  CSB ========================
      if(jk.eq.3)then
      uql(1)=uql(3)+tl(13)*rp(1,3)+dl1c*(rp(4,3)+rp(2,3)/r)-
     -dl4c*(rp(3,3)+2.*rp(2,3)/r)
      uql(2)=uql(3)-tl(13)*rp(1,3)-dl1c*(rp(4,3)+rp(2,3)/r)+
     +dl4c*(rp(3,3)+2.*rp(2,3)/r)
      else
      uql(4)=uql(4)+tl(13)*(rp(1,1)-rp(1,2))+
     +dl1c*((rp(4,1)-rp(4,2))+(rp(2,1)-rp(2,2))/r)-
     -dl4c*((rp(3,1)-rp(3,2))+2.*(rp(2,1)-rp(2,2))/r)
      end if
c======================================================
      do 45 ji=1,2
      amb(ji,jn)=d1*rp(ji,4)+d3*rp(ji,jn)+dl1*rp(ji,3)
c=======================  CSB ========================
      amb(ji,jn)=amb(ji,jn)-0.25*(tl(14)+tl(15))*rp(ji,3)*(-1)**(jn)
c================== frozen core  ======================
c	amb(ji,jn)=d1*rp(ji,4)+d3*rp(ji,jn)
c======================================================
   45 continue
   46 continue
      ul3=.25*tl(4)*(rp(1,4)**2+2.*rp(1,1)*rp(1,2))
      if(tl(12).eq.0.)ul3=tl(4)*rll
      if(tl(12).eq.1.)ul3=tl(4)/2.*rmm*tl(9)*(rp(1,4)**2-
     -.5*(rp(1,1)**2+rp(1,2)**2))
      uq2=t(1)*d5*rp(1,4)-d4*rp(3,4)-(2.*d4-d1)*rp(2,4)/r+
     +d1*rp(4,4)-.5*t(6)*(rp(6,4)+2./r*rp(5,4))
      do 42 j=1,2
      amq(j)=const(j)+amb(1,j)
      if(i.eq.n1)amm(j)=amq(j)
      beta(j)=-amb(2,j)/amq(j)
   42 continue
      l1=ln-1
      if(l(3).eq.0)l1=ln
      do 43 im=1,l1
      j=1
      lp=l(1)
      if(im.gt.lp)j=2
      vc1=0.
      jq=2
      if(j.eq.2)jq=1
      if(j.eq.1)vc1=vc
      dd=t(7)
      ral=1.
      rall=1.
      if(dd.ne.0.)rall=rp(1,4)**dd
      if(d9.ne.0.)ral=rp(1,4)**d9
      uq3=t(4)/6.*(rall*(rp(1,4)*d7-rp(1,j)*d8)+
     +.5*t(7)*ral*(rp(1,4)**2*d7-(rp(1,1)**2+rp(1,2)**2)*d8))
      vc1=vc1+uq3
      ca=aj(im)*(aj(im)+1.)-al(im)*(al(im)+1.)-.75
      do 49 jn=1,2
      jk=jn+2
      aa=1.
      if(jk.eq.4)aa=-1.
c     (1.21) in phd
      ulw(jk)=(.5*(tl(10)+aa*tl(11))*rp(2,jk)/r-
     -(tl(2)*tl(6)+tl(3)*tl(7))/8.*rp(5,jk)/r)*ca
      uqw(jn)=ca/r*(.5*t(6)*rp(2,4)+.5*t(16)*rp(2,jn)+
Cnorm uqw(jn)=ca/r*(.5*t(6)*(rp(2,4)+rp(2,jn))+
c=====================  J-terms  ======================
     +.125*(t(2)-t(3))*rp(5,jn)*t(13)-
     -.125*(t(2)*t(9)+t(3)*t(10))*rp(5,4)*t(13)+
c===================  tensor terms  ===================
     +(t(14)-t(15))*rp(5,jn)+t(15)*rp(5,4))
c======================================================
   49 continue
cccccccccccccccccc
      ufr=uq2+uq1(j)+vc1
c================== neutron halo =====================
c	 if(im.eq.ln)ufr=ufr*1.1944
c	if(im.eq.ln-1)ufr=ufr*1.0
c================== proton halo ======================
      ufr1=uq2+uq1(j)+uq3
        if(im.eq.l(1))ufr1=ufr1*1.0
        if(im.eq.l(1))ufr=ufr1+vc
ccccccccccccccccccccc
      if (im.le.l(1)) then
      uv(im)=(ufr+uqw(j)+uql(1)+ulw(3)+u3(j)-               ! CSB taken into account
     -en(im))/amq(j)
c     print*, uql(1), uql(3), uv(im); pause
      else
      uv(im)=(ufr+uqw(j)+uql(2)+ulw(3)+u3(j)-
     -en(im))/amq(j)
      end if
c============== frozen core =================
c      uv(im)=(ufr+uqw(j)-en(im))/amq(j)
c============================================
      uvh(im)=uv(im)+en(im)/amq(j)
      if (im.le.l(1)) then                                 ! CSB takein into account
      uvb(im)=ufr+u3(j)+uql(1)
	upp(im)=uq2+uq1(j)+uq3+u3(j)+uql(1)
	alfa(im)=al(im)*(al(im)+1.)/r**2+(ufr+uqw(j)+u3(j)+uql(1)+
     +ulw(3)-en(im))/amq(j)
      else
      uvb(im)=ufr+u3(j)+uql(2)
	upp(im)=uq2+uq1(j)+uq3+u3(j)+uql(2)
	alfa(im)=al(im)*(al(im)+1.)/r**2+(ufr+uqw(j)+u3(j)+uql(2)+
     +ulw(3)-en(im))/amq(j)
      end if
   43 continue
c============== frozen core =================
c	uvb(im)=ufr
c	upp(im)=uq2+uq1(j)+uq3
c   43 alfa(im)=al(im)*(al(im)+1.)/r**2+(ufr+uqw(j)-en(im))/amq(j)
c============================================
      if(l(3).eq.0)return
      amq(3)=const2+aml(1)
      if(i.eq.n1)amm(3)=amq(3)
      beta(3)=-aml(2)/amq(3)
      cal=aj(ln)*(aj(ln)+1.)-al(ln)*(al(ln)+1.)-.75
      ulw(1)=(.5*(tl(10)-tl(11))*rp(2,4)/r-(tl(2)*tl(6)+
     +tl(3)*tl(7))*rp(5,4)/r/8.)*cal
      co=al(ln)*(al(ln)+1.)/r**2
      uv(ln)=(uql(4)+ul3+ulw(1)-en(ln))/amq(3)
      uvh(ln)=uv(ln)+en(im)/amq(3)
      uvb(ln)=uql(4)+ul3
	  upp(ln)=uql(4)+ul3
      alfa(ln)=(uql(4)+ul3+ulw(1)-en(ln))/amq(3)+co
      return
      end
c
      function could(r)
      common/rr/rr1(800,6,3),rr2(800,6,3)
      common/param/h1,h2,ht,rt,n,n0,n1
      b=(n0-1)*h1+(n-n0)*h2
      n2=n0-2
      k1=1
      h=h1
      rk=0.
      e=1/137.036
      could=0.
   55 coul=0.
      do 53 ih=k1,n2,2
      rk1=rk+h
      rk2=rk1+h
      rop1=rr1(ih,1,1)*4.*3.141593*rk*rk
      ih1=ih+1
      rop2=rr1(ih1,1,1)*4.*3.141593*rk1*rk1
      ih2=ih+2
      rop3=rr1(ih2,1,1)*4.*3.141593*rk2*rk2
      if(rk.le.r)rk=r
      f1=e/rk*rop1
      if(rk1.le.r)rk1=r
      f2=e/rk1*rop2
      rk=rk2
      if(rk2.le.r)rk2=r
      f3=e/rk2*rop3
   53 coul=coul+f1/6.+2./3.*f2+f3/6.
      could=coul*h*2.+could
      if(h.ge.h2)return
      k1=n0
      n2=n-2
      h=h2
      na=n-n0-1
      if(na/2*2.eq.na)n2=n-3
      go to 55
      end
c
      subroutine prot(i,r,rp)
      dimension rp(6,4)
      common/aj/aj(40)/al/al(40)/lwf/l(3),alo(40),ln
      common/rf/rf(800,40)/rf1/rf1(800,40),rf2(800,40)
      j1=0
      do 31 il=1,3
      do 98 im=1,6
   98 rp(im,il)=0.
      l1=l(il)
      if(l1.eq.0)goto 31
      do 32 j=1,l1
      j1=j1+1
      ajj=alo(j1)
      rff=rf(i,j1)**2
      rfd=rf(i,j1)*rf1(i,j1)*r
      al1=al(j1)*(al(j1)+1.)
      aj1=aj(j1)*(aj(j1)+1.)
      rp(1,il)=rp(1,il)+ajj*rff
      rp(2,il)=rp(2,il)+ajj*(rfd-rff)
      rp(3,il)=rp(3,il)+ajj*(3.*rff-4.*rfd+rf1(i,j1)**2*r**2+r**2*
     *rf(i,j1)*rf2(i,j1))
      rp(4,il)=rp(4,il)+ajj*((rf1(i,j1)*r-rf(i,j1))**2+al1*rff)
      rp(5,il)=rp(5,il)+ajj*(aj1-al1-.75)*rff
   32 rp(6,il)=rp(6,il)-ajj*(aj1-al1-.75)*(3.*rff-2.*rfd)
      v=4.*3.141593*r*r
      rp(1,il)=rp(1,il)/v
      rp(2,il)=rp(2,il)/v/r*2.
      rp(3,il)=rp(3,il)/v/r**2*2.
      rp(5,il)=rp(5,il)/v/r
      rp(6,il)=rp(6,il)/v/r**2
      rp(4,il)=rp(4,il)/v/r**2
31	continue
      return
      end
c
      subroutine solv(i,r,h)
      dimension y(20)
      external extern
      common/lwf/l(3),alo(40),ln/index/ind
      common/coeff/alf1(2,40),bet1(2,3)
      common/rf/rf(800,40)/rf1/rf1(800,40),rf2(800,40)/jj1/j1,in
      common/yy1/y1(2,40)/rae/hb,rb
      common/param/h1,h2,ht,rt,n,n0,n1
      j1=0
      do 11 in=1,3
      l1=l(in)
      if(l1.eq.0)goto 11
      do 12 j=1,l1
      j1=j1+1
      fc=1.
      if(ind.eq.2)fc=-1.
      rf(i,j1)=y1(1,j1)
      rf1(i,j1)=y1(2,j1)
      rf2(i,j1)=alf1(1,j1)*rf(i,j1)+bet1(1,in)*rf1(i,j1)
      y(1)=rf(I,j1)
      y(2)=fc*rf1(i,j1)
      ra=r
      hb=h
      rb=r
	if (i.eq.n.and.alf1(1,j1).lt.0)write(*,1)
1	format(2x,'sqrt_from_neg_alf1(1,j1)')
      if(i.eq.n)y(2)=rf(i,j1)*sqrt(alf1(1,j1))
      call intstp(2,h,ra,y,extern)
      y1(1,j1)=y(1)
   12 y1(2,j1)=y(2)*fc
   11 continue
      return
      end
c
      subroutine extern(x,y,f)
      dimension y(20),f(20)
      common/coeff/alf1(2,40),bet1(2,3)/index/ind/jj1/j1,in/rae/hb,rb
      fa=1.
      if(ind.eq.2)fa=-1.
      al=alf1(1,j1)
      be=bet1(1,in)
      if(abs(x-rb-hb/2.).lt.1e-6)al=(alf1(1,j1)+alf1(2,j1))/2.
      if(abs(x-hb/2.-rb).lt.1e-6)be=(bet1(1,in)+bet1(2,in))/2.
      if(abs(x-rb-hb).lt.1e-6)al=alf1(2,j1)
      if(abs(x-rb-hb).lt.1e-6)be=bet1(2,in)
      f(1)=y(2)
      f(2)=y(1)*al+fa*y(2)*be
      return
      end
c
      SUBROUTINE INTSTP(NN,HH,XX,Y,EXTERN)
      DIMENSION Y(20),G(20),O(20),Z(20)
      N=NN
      H=HH
      H1=0.5*H
      H2=H/6.0
      X=XX
      XHH=X+H1
      XH=X+H
      CALL EXTERN(X,Y,G)
      DO 2 J=1,N
   2  Z(J)=Y(J)+H1*G(J)
      CALL EXTERN(XHH,Z,O)
       DO 3 J=1,N
       G(J)=G(J)+2.0*O(J)
   3   Z(J)=Y(J)+H1*O(J)
      CALL EXTERN(XHH,Z,O)
       DO 5 J=1,N
       G(J)=G(J)+2.0*O(J)
   5   Z(J)=Y(J)+H*O(J)
      CALL EXTERN(XH,Z,O)
      DO 6 J=1,N
   6  Y(J)=Y(J)+H2*(G(J)+O(J))
      XX=XH
      RETURN
      END
c
      subroutine solv2
      external extern
      common/en1/en1(40)
      common/rf/rf(800,40)/rf1/rf1(800,40),rf2(800,40)
      dimension y(20)
      common/param/h1,h2,ht,rt,n,n0,n1
      common/lwf/l(3),alo(40),ln
      common/coef/alfa(40),beta(3)
      common/en/en(40)/r/ro(6,40)/JJ1/j1,in/rae/hb,rb/amm/amm(3)
      j1=0
	nb=n+2
	na=n+1
      do 22 in=1,3
      l1=l(in)
      if(l1.eq.0)goto 22
      do 21 j=1,l1
      j1=j1+1
      rk=-h1*n1
      y(1)=rf(n1+1,j1)
      y(2)=-rf1(n1+1,j1)
      ind=2
      rb=rk
      hb=h1
      call intstp(2,h1,rk,y,extern)
      y(2)=-y(2)
      b1=h1*(n1-1)
      call simp(0.,b1,h1,pc1)
      b2=h1*(n0-1)
      b1=b1+h1
      call simp(b1,b2,h1,pc21)
      pc21=pc21+y(1)*y(1)*h1
      b=h1*(n0-1)+(n-n0)*h2
      call simp(b2,b,h2,pc22)
      pc2=pc21+pc22
	ppcc=pc2+pc1*y(1)**2/rf(n1,j1)**2
	write (*,1) ppcc
1	format (2x,'SOLV2',3x,'ppcc=',e15.5)
      c2=sqrt(1./(pc2+pc1*y(1)**2/rf(n1,j1)**2))
      c1=c2*y(1)/rf(n1,j1)
      f=rf1(n1,j1)/rf(n1,j1)-y(2)/y(1)
      f1=1./amm(in)*(pc1/rf(n1,j1)**2+pc2/y(1)**2)
      do 234 jk=na,nb
      rf(jk,j1)=rf(jk,j1)*c1
      rf1(jk,j1)=rf1(jk,j1)*c1
      rf2(jk,j1)=rf2(jk,j1)*c1
  234 continue
      do 24 jm=1,n
      if(jm.gt.n1)goto 25
      rf(jm,j1)=rf(jm,j1)*c1
      rf1(jm,j1)=rf1(jm,j1)*c1
      rf2(jm,j1)=rf2(jm,j1)*c1
      goto 24
   25 rf(jm,j1)=rf(jm,j1)*c2
      rf1(jm,j1)=rf1(jm,j1)*c2
      rf2(jm,j1)=rf2(jm,j1)*c2
   24 continue
      en1(j1)=f/f1
      do 651 im=1,6
  651 ro(im,j1)=ro(im,j1)*c1
   21 continue
   22 continue
      return
      end
c
      Subroutine simp(a,b,h,pc)
      common/param/h1,h2,ht,rt,n,n0,n1
      common/jj1/j1,in
      common/rf/rf(800,40)
      pc=0.
      ns=(b-a)/h-.9
      nk=a/h1+.1
      n1s=ns
      if(ns/2*2.eq.ns)n1s=ns+1
      r=a
      do 61 jl=1,n1s,2
      jk=jl+nk
      r=r+2.*h
      rc=rf(jk,j1)**2/6.+2./3.*rf(jk+1,j1)**2+rf(jk+2,j1)**2/6.
      if(r.gt.b*1.00001)rc=rf(jk,j1)**2/6.+rf(jk+1,j1)**2/3.
   61 pc=pc+rc
      pc=pc*h*2.
      return
      end
c
c
c
      subroutine teyl
      common/rf/rf(800,40)/rf1/rf1(800,40),rf2(800,40)
      common/al/al(40)/aj/aj(40)/en/en(40)/y/y2(2,40)
      common/param/h1,h2,ht,rt,n,n0,n1
      common/lwf/l(3),alo(40),ln
      common/coef1/alf2(2,40),bet2(2,3)/r/r0(6,40)
      jl=0
      r=0.
      do 85 jm=1,3
      l1=l(jm)
      if(l1.eq.0)goto 85
      do 86 jn=1,l1
      jl=jl+1
      if(al(jl).eq.0.)r0(2,jl)=(alf2(1,jl)+bet2(1,jm))*r0(1,jl)
      if(al(jl).eq.0.)r0(3,jl)=(3.*alf2(2,jl)+bet2(2,jm))*r0(1,jl)+
     +(alf2(1,jl)+3.*bet2(1,jm))*r0(2,jl)
      if(al(jl).eq.1.)r0(2,jl)=(alf2(1,jl)+2.*bet2(1,jm))*r0(1,jl)*1.2
      if(al(jl).eq.1.)r0(3,jl)=((6.*alf2(2,jl)+4.*bet2(2,jm))*r0(1,jl)+
     +(alf2(1,jl)+4.*bet2(1,jm))*r0(2,jl))*15./14.
      if(al(jl).eq.2.)r0(2,jl)=(alf2(1,jl)+3.*bet2(1,jm))*r0(1,jl)*10/7.
      if(al(jl).eq.2.)r0(3,jl)=(alf2(2,jl)+bet2(2,jm))*r0(1,jl)*35./3.+
     +(alf2(1,jl)+5.*bet2(1,jm))*r0(2,jl)*7./6.
      if(al(jl).eq.3.)r0(2,jl)=(alf2(1,jl)+4.*bet2(1,jm))*r0(1,jl)*5./3.
      if(al(jl).eq.3.)r0(3,jl)=(3.*alf2(2,jl)+4.*bet2(2,jm))*r0(1,jl)*
     *70./11.+(alf2(1,jl)+6.*bet2(1,jm))*r0(2,jl)*14./11.
      if(al(jl).eq.4.)r0(2,jl)=(alf2(1,jl)+5.*bet2(1,jm))*r0(1,jl)*
     *21./11.
      if(al(jl).eq.4.)r0(3,jl)=(21.*alf2(2,jl)+35.*bet2(2,jm))*
     *r0(1,jl)*18./13.+(alf2(1,jl)+7.*bet2(1,jm))*r0(2,jl)*18./13.
      if(al(jl).eq.5.)r0(2,jl)=(alf2(1,jl)+6.*bet2(1,jm))*r0(1,jl)*
     *28./13.
      if(al(jl).eq.5.)r0(3,jl)=((28.*alf2(2,jl)+56.*bet2(2,jm))*
     *r0(1,jl)+(alf2(1,jl)+8.*bet2(1,jm))*r0(2,jl))*1.5
      if(al(jl).eq.6.)r0(2,jl)=(alf2(1,jl)+7.*bet2(1,jm))*r0(1,jl)*
     *12./5.
      if(al(jl).eq.6.)r0(3,jl)=((36.*alf2(2,jl)+84.*bet2(2,jm))*
     *r0(1,jl)+(alf2(1,jl)+9.*bet2(1,jm))*r0(2,jl))*55./34.
      if(al(jl).eq.7.)r0(2,jl)=(alf2(1,jl)+8.*bet2(1,jm))*r0(1,jl)*
     *45./17.
      if(al(jl).eq.7.)r0(3,jl)=((45.*alf2(2,jl)+120.*bet2(2,jm))*
     *r0(1,jl)+(alf2(1,jl)+10.*bet2(1,jm))*r0(2,jl))*33./19.
      if(al(jl).eq.8.)r0(2,jl)=(alf2(1,jl)+9.*bet2(1,jm))*r0(1,jl)*
     *55./19.
      if(al(jl).eq.8.)r0(3,jl)=((55.*alf2(2,jl)+165.*bet2(2,jm))*
     *r0(1,jl)+(alf2(1,jl)+11.*bet2(1,jm))*r0(2,jl))*39./21.
      if(al(jl).eq.9.)r0(2,jl)=(alf2(1,jl)+10.*bet2(1,jm))*r0(1,jl)*
     *66./21.
      if(al(jl).eq.9.)r0(3,jl)=((66.*alf2(2,jl)+220.*bet2(2,jm))*
     *r0(1,jl)+(alf2(1,jl)+12.*bet2(1,jm))*r0(2,jl))*91./46.
c      write(*,566)r0(1,jl),r0(2,jl),r0(3,jl)
  566 format(5x,3e10.3)
   86 continue
   85 continue
      do 70 ii=4,6
      do 70 ii1=1,ln
   70  r0(ii,ii1)=0.
       nr=rt/h1+.1
      ki=1
      h=h1
  672 continue
      do 99 i=ki,nr
      do 98 jc=1,ln
      if(al(jc).eq.0.)rf(i,jc)=r0(1,jc)*r+r0(2,jc)*r**3/6.+
     +r0(3,jc)*r**5/120.
      if(al(jc).eq.0.)rf1(i,jc)=r0(1,jc)+r0(2,jc)*r**2/2.+
     +r0(3,jc)*r**4/24.
      if(al(jc).eq.0.)rf2(i,jc)=r0(2,jc)*r+r0(3,jc)*r**3/6.
      if(al(jc).eq.1.)rf1(i,jc)=r0(1,jc)*r+r0(2,jc)*r**3/6.+
     +r0(3,jc)*r**5/120.
      if(al(jc).eq.1.)rf2(i,jc)=r0(1,jc)+r0(2,jc)*r**2/2.+
     +r0(3,jc)*r**4/24.
      if(al(jc).eq.1.)rf(i,jc)=r0(1,jc)*r**2/2.+r0(2,jc)*r**4/24.+
     +r0(3,jc)*r**6/720.
      if(al(jc).eq.2.)rf2(i,jc)=r0(1,jc)*r+r0(2,jc)*r**3/6.+
     +r0(3,jc)*r**5/120.
      if(al(jc).eq.2.)rf(i,jc)=r0(1,jc)*r**3/6.+r0(2,jc)*r**5/120.+
     +r0(3,jc)*r**7/5040.
      if(al(jc).eq.2.)rf1(i,jc)=r0(1,jc)*r**2/2.+r0(2,jc)*r**4/24.+
     +r0(3,jc)*r**6/720.
      if(al(jc).eq.3.)rf(i,jc)=r0(1,jc)*r**4/24.+r0(2,jc)*r**6/720.+
     +r0(3,jc)*r**8/40320.
      if(al(jc).eq.3.)rf1(i,jc)=r0(1,jc)*r**3/6.+r0(2,jc)*r**5/120.+
     +r0(3,jc)*r**7/5040.
      if(al(jc).eq.3.)rf2(i,jc)=r0(1,jc)*r**2/2.+r0(2,jc)*r**4/24.+
     +r0(3,jc)*r**6/720.
      if(al(jc).eq.4.)rf1(i,jc)=r0(1,jc)*r**4/24.+r0(2,jc)*r**6/720.+
     +r0(3,jc)*r**8/40320.
      if(al(jc).eq.4.)rf2(i,jc)=r0(1,jc)*r**3/6.+r0(2,jc)*r**5/120.+
     +r0(3,jc)*r**7/5040.
      if(al(jc).eq.4.)rf(i,jc)=r0(1,jc)*r**5/120.+r0(2,jc)*r**7/5040.+
     +r0(3,jc)*r**9/362880.
      if(al(jc).eq.5.)rf2(i,jc)=r0(1,jc)*r**4/24.+r0(2,jc)*r**6/720.+
     +r0(3,jc)*r**8/40320.
      if(al(jc).eq.5.)rf(i,jc)=r0(1,jc)*r**6/720.+r0(2,jc)*r**8/40320.+
     +r0(3,jc)*r**10/3628800.
      if(al(jc).eq.5.)rf1(i,jc)=r0(1,jc)*r**5/120.+r0(2,jc)*r**7/5040.+
     +r0(3,jc)*r**9/362880.
      if(al(jc).eq.6.)rf(i,jc)=r0(1,jc)*r**7/5040.+
     +r0(2,jc)*r**9/362880.+r0(3,jc)*r**11/39916800.
      if(al(jc).eq.6.)rf1(i,jc)=r0(1,jc)*r**6/720.+r0(2,jc)*r**8/40320.+
     +r0(3,jc)*r**10/3628800.
      if(al(jc).eq.6.)rf2(i,jc)=r0(1,jc)*r**5/120.+r0(2,jc)*r**7/5040.+
     +r0(3,jc)*r**9/362880.
      if(al(jc).eq.7.)rf1(i,jc)=r0(1,jc)*r**7/5040.+
     +r0(2,jc)*r**9/362880.+r0(3,jc)*r**11/39916800.
      if(al(jc).eq.7.)rf2(i,jc)=r0(1,jc)*r**6/720.+r0(2,jc)*r**8/40320.+
     +r0(3,jc)*r**10/3628800.
      if(al(jc).eq.7.)rf(i,jc)=r0(1,jc)*r**8/440320.+
     +r0(2,jc)*r**10/3628800.+r0(3,jc)*r**12/4.79e8
      if(al(jc).eq.8.)rf2(i,jc)=r0(1,jc)*r**7/5040.+
     +r0(2,jc)*r**9/362880.+r0(3,jc)*r**11/39916800.
      if(al(jc).eq.8.)rf(i,jc)=r0(1,jc)*r**9/362880.+
     +r0(2,jc)*r**11/39916800.+r0(3,jc)*r**13/6.22702e9
      if(al(jc).eq.8.)rf1(i,jc)=r0(1,jc)*r**8/440320.+
     +r0(2,jc)*r**10/3628800.+r0(3,jc)*r**12/4.79e8
      if(al(jc).eq.9.)rf(i,jc)=r0(1,jc)*r**10/3628800.+
     +r0(2,jc)*r**12/4.79e8+r0(3,jc)*r**14/8.71782e10
      if(al(jc).eq.9.)rf1(i,jc)=r0(1,jc)*r**9/362880.+
     +r0(2,jc)*r**11/39916800.+r0(3,jc)*r**13/6.22702e9
      if(al(jc).eq.9.)rf2(i,jc)=r0(1,jc)*r**8/440320.+
     +r0(2,jc)*r**10/3628800.+r0(3,jc)*r**12/4.79e8
   98 continue
   99 r=r+h
      if(ki.eq.n+1)goto 451
      ki=n+1
      nr=n+2
      h=ht
      r=ht
      goto 672
  451 r=rt
      do 97 jc=1,ln
      if(al(jc).eq.0.)y2(1,jc)=r0(1,jc)*r+r0(2,jc)*r**3/6.+
     +r0(3,jc)*r**5/120.
      if(al(jc).eq.0.)y2(2,jc)=r0(1,jc)+r0(2,jc)*r**2/2.+
     +r0(3,jc)*r**4/24.
      if(al(jc).eq.1.)y2(2,jc)=r0(1,jc)*r+r0(2,jc)*r**3/6.+
     +r0(3,jc)*r**5/120.
      if(al(jc).eq.1.)y2(1,jc)=r0(1,jc)*r**2/2.+r0(2,jc)*r**4/24.+
     +r0(3,jc)*r**6/720.
      if(al(jc).eq.2.)y2(1,jc)=r0(1,jc)*r**3/6.+r0(2,jc)*r**5/120.+
     +r0(3,jc)*r**7/5040.
      if(al(jc).eq.2.)y2(2,jc)=r0(1,jc)*r**2/2.+r0(2,jc)*r**4/24.+
     +r0(3,jc)*r**6/720.
      if(al(jc).eq.3.)y2(1,jc)=r0(1,jc)*r**4/24.+r0(2,jc)*r**6/720.+
     +r0(3,jc)*r**8/40320.
      if(al(jc).eq.3.)y2(2,jc)=r0(1,jc)*r**3/6.+r0(2,jc)*r**5/120.+
     +r0(3,jc)*r**7/5040.
      if(al(jc).eq.4.)y2(2,jc)=r0(1,jc)*r**4/24.+r0(2,jc)*r**6/720.+
     +r0(3,jc)*r**8/40320.
      if(al(jc).eq.4.)y2(1,jc)=r0(1,jc)*r**5/120.+r0(2,jc)*r**7/5040.+
     +r0(3,jc)*r**9/362880.
      if(al(jc).eq.5.)y2(1,jc)=r0(1,jc)*r**6/720.+r0(2,jc)*r**8/40320.+
     +r0(3,jc)*r**10/3628800.
      if(al(jc).eq.5.)y2(2,jc)=r0(1,jc)*r**5/120.+r0(2,jc)*r**7/5040.+
     +r0(3,jc)*r**9/362880.
      if(al(jc).eq.6.)y2(1,jc)=r0(1,jc)*r**7/5040.+
     +r0(2,jc)*r**9/362880.+r0(3,jc)*r**11/39916800.
      if(al(jc).eq.6.)y2(2,jc)=r0(1,jc)*r**6/720.+r0(2,jc)*r**8/40320.+
     +r0(3,jc)*r**10/3628800.
      if(al(jc).eq.7.)y2(2,jc)=r0(1,jc)*r**7/5040.+
     +r0(2,jc)*r**9/362880.+r0(3,jc)*r**11/39916800.
      if(al(jc).eq.7.)y2(1,jc)=r0(1,jc)*r**8/440320.+
     +r0(2,jc)*r**10/3628800.+r0(3,jc)*r**12/4.79e8
      if(al(jc).eq.8.)y2(1,jc)=r0(1,jc)*r**9/362880.+
     +r0(2,jc)*r**11/39916800.+r0(3,jc)*r**13/6.22702e9
      if(al(jc).eq.8.)y2(2,jc)=r0(1,jc)*r**8/440320.+
     +r0(2,jc)*r**10/3628800.+r0(3,jc)*r**12/4.79e8
      if(al(jc).eq.9.)y2(1,jc)=r0(1,jc)*r**10/3628800.+
     +r0(2,jc)*r**12/4.79e8+r0(3,jc)*r**14/8.71782e10
      if(al(jc).eq.9.)y2(2,jc)=r0(1,jc)*r**9/362880.+
     +r0(2,jc)*r**11/39916800.+r0(3,jc)*r**13/6.22702e9
   97 continue
c      write(*,577)y2
c 577 format(5x,6e10.3)
      return
      end
c
      subroutine result(CP,GP,CN,GN)
      external could
      common/sl/const(2),cons,const2
      common/tsk/t(16),tl(15)/ass/pmn(2),pml,aaa
      common/rf/rf(800,40)/rf1/rf1(800,40),rf2(800,40)
      dimension uvv(40)
      dimension uq1(2)
      common/al/al(40)/aj/aj(40)/en/en(40)/ki/k
      common/param/h1,h2,ht,rt,n,n0,n1/skr/skr(40),sk
      common/lwf/l(3),alo(40),ln
      common/coef/alfa(40),beta(3)
      common/rr/rr1(800,6,3),rr2(800,6,3)
      dimension rp(6,4)

      double precision :: CP,CN,GP,GN
		hg1=0.
		hg2=0.
		hg3=0.
		hg4=0.
		hg5=0.
		hg6=0.
		hg7=0.
      tt=0.
      hh=0.
      sk=0.
        sk1=0.
		sk2=0.
		sk11=0.
        blp=0.
      tp=0.
      tp11=0.
      r=h1
      nk=n0
      kj=2
      h=h1
		na=n+1
		nb=n+2
  221 continue
      do 222 i=kj,nk
      call prot(i,r,rp)
      do 223 j1=1,3
      do 223 j2=1,6
      rr1(i,j2,j1)=rp(j2,j1)
      rr2(i,j2,j1)=rp(j2,j1)
  223 continue
  222 r=r+h
      if(kj.gt.2)goto 205
      r=r-h+h2
      nk=n
      kj=n0+1
      h=h2
      goto 221
  205 if(kj.eq.na)goto 645
      r=ht
      nk=nb
      kj=na
      h=ht
      goto 221
  645 hp=1.
      e=1./137.036
      const3=-e*(3./3.141593)**.3333
      d1=.25*(t(2)*(1.+.5*t(9))+t(3)*(1.+.5*t(10)))
      d2=.125*(3.*t(2)*(1.+2.*t(9))+t(3)*(1.+2.*t(10)))
      d3=.125*(t(3)*(1.+2.*t(10))-t(2)*(1.+2.*t(9)))
      d4=.125*(3.*t(2)*(1.+.5*t(9))-t(3)*(1.+.5*t(10)))
      d5=1.+.5*t(5)
      d6=t(5)+.5
      dl1=.25*(tl(2)*(tl(6)/2.+1.)+tl(3)*(tl(7)/2.+1.))
      dl2=tl(9)+1.
      dl3=tl(9)
      rl=1.
      rll=1.
      dl0=tl(9)-1.
      do 25 im=1,40
   25 skr(im)=0.
      r=h1
      nk=n0
      k=0
      h=h1
    5 j1=2-k
      do 1 in=j1,nk
      i=in+n0*k-k
      do 41 ij=1,6
   41 rp(ij,4)=rr1(i,ij,1)+rr1(i,ij,2)
      if(dl2.ne.0.)rll=rp(1,4)**dl2
      if(dl3.ne.0.)rl=rp(1,4)**dl3
      if(dl0.ne.0..and.rr1(i,1,3).ne.0.)rmm=rr1(i,1,3)**dl0
      if(dl3.ne.0..and.rr1(i,1,3).ne.0.)rm=rr1(i,1,3)**dl3
      vc=could(r)
      dl4=.125*(3.*tl(2)*(tl(6)/2.+1.)-tl(3)*(tl(7)/2.+1.))
c     (1.13 ?) in phd
      ul2=const2*rr1(i,4,3)+rr1(i,1,3)*tl(1)*rp(1,4)+
     +(rp(4,4)*rr1(i,1,3)+rp(1,4)*rr1(i,4,3))*dl1-
     -dl4*rr1(i,1,3)*(rp(3,4)+2./r*rp(2,4))+
     +tl(10)/2.*(rp(2,4)*rr1(i,5,3)+rr1(i,2,3)*rp(5,4))+
     +tl(11)/2.*(rp(2,4)*rr1(i,5,3)-rr1(i,2,3)*rp(5,4))-
     -(tl(2)*tl(6)+tl(3)*tl(7))/8.*rp(5,4)*rr1(i,5,3)
c=======================  CSB  ========================
      ul2=ul2+tl(13)*rr1(i,1,3)*(rr1(i,1,1)-rr1(i,1,2))+
     +0.25*(tl(14)+tl(15))*(rr1(i,4,1)-rr1(i,4,2))*rr1(i,1,3)+
     +0.25*(tl(14)+tl(15))*(rr1(i,1,1)-rr1(i,1,2))*rr1(i,4,3)-
     -0.125*(3.*tl(14)-tl(15))*rr1(i,1,3)*(rr1(i,3,1)-rr1(i,3,2))-
     -0.125*(3.*tl(14)-tl(15))*rr1(i,1,3)*2./r*(rr1(i,2,1)-rr1(i,2,2))
c======================================================
      do 345 jn=1,2
      uq1(jn)=(-t(1)*d6*rr1(i,1,jn)+d2*.5*rr1(i,3,jn)+d2*rr1(i,2,jn)/
     /r+2.*d3*rr1(i,4,jn))*rr1(i,1,jn)/2.+.5*t(16)*rr1(i,5,jn)*rr1(i,2,
Cnorm /r+2.*d3*rr1(i,4,jn))*rr1(i,1,jn)/2.+.5*t(6)*rr1(i,5,jn)*rr1(i,2,
     +jn)+const(jn)*rr1(i,4,jn)+
c=====================  J-terms  ======================
     +.0625*(t(2)-t(3))*(rr1(i,5,jn)**2)*t(13)+
c===================  tensor terms  ===================
     +0.5*(t(14)-t(15))*rr1(i,5,jn)**2
c======================================================
  345 continue
      ul3=.25*tl(4)*(rp(1,4)**2+2.*rr1(i,1,1)*rr1(i,1,2))*rr1(i,1,3)
      if(tl(12).eq.0.)ul3=tl(4)*rll*rr1(i,1,3)
      if(tl(12).eq.1.)ul3=tl(4)/2.*rm*(rp(1,4)**2-
     -.5*(rr1(i,1,1)**2+rr1(i,1,2)**2))
      uq2=(t(1)*d5*rp(1,4)-d4*rp(3,4)-2.*d4*rp(2,4)/r+
     +d1*2.*rp(4,4))*rp(1,4)/2.+.5*t(6)*rp(5,4)*rp(2,4)-
c=====================  J-terms  ======================
     -.0625*(t(2)*t(9)+t(3)*t(10))*((rr1(i,5,1)+rr1(i,5,2))**2)*t(13)+
c===================  tensor terms  ===================
     +0.5*t(15)*(rr1(i,5,1)+rr1(i,5,2))**2
c======================================================
      ral=1.
      d9=t(7)
      if(d9.ne.0.)ral=rp(1,4)**d9
      d7=1.+.5*t(8)
      d8=.5+t(8)
      tn=const(1)*.5*(rr1(i,4,1)-.5*rr1(i,3,1)-rr1(i,2,1)/r)+
     +const(2)*.5*(rr1(i,4,2)-.5*rr1(i,3,2)-rr1(i,2,2)/r)
      tn11=const(1)*.5*rr1(i,4,1)+const(2)*.5*rr1(i,4,2)
		tncm=.5/pmn(1)*rr1(i,4,1)+rr1(i,4,2)/pmn(2)/2.
      tn3=-t(4)*ral*(rp(1,4)**2/2.*d7-(rr1(i,1,1)**2+rr1(i,1,2)**2)/2.*
     *d8)*d9/12.
      tl1=const2*.5*(rr1(i,4,3)-rr1(i,3,3)/2.-rr1(i,2,3)/r)
      tl11=const2*.5*rr1(i,4,3)
      tl3=-.5*ul3
      if(tl(12).le.1.)tl3=tl3*dl3
cccccccc_exchange Coulomb
		vcex=.75*const3*rr1(i,1,1)**1.3333
c		vcex=0.
ccccccccc
      hn=uq1(1)+uq1(2)+uq2+vc*rr1(i,1,1)/2.+vcex-2.*tn3/d9
      hl=ul3+ul2
		sim=4./3.*h
		if(i/2*2.ne.i)sim=sim/2.
		if(i.eq.1.or.i.eq.n.or.i.eq.n0)sim=h/3.
		hg1=hg1+(hn+hl)*r*r*sim
		hg2=hg2+hn*r*r*sim
		hg3=hg3+(tn+tn3+tl1+tl3+1./3.*vcex)*r*r*sim
		hg4=hg4+(tn11+tn3+tl11+tl3+1./3.*vcex)*r*r*sim
		hg6=hg6+(hn+hl+ull)*r*r*sim
		hg7=hg7+tncm*r*r*sim
cccccccccccccccccccccccccccccccccccccccccccccccccccc
c      hg(i,1)=(hn+hl)*r*r
c      hg(i,2)=tn*r*r
c      hg(i,3)=(tn+tn3+tl1+tl3+1./3.*vcex)*r*r
c      hg(i,4)=(tn11+tn3+tl11+tl3)*r*r
cccccccccccccccccccccccccccccccccccccccccccccccccccc
      do 135 ip=1,ln
  135 skr(ip)=skr(ip)+rf(i,ip)**2*r*r*sim
      lnn=ln-l(3)
      do 235 ip=1,lnn
  235 hg5=hg5+rf(i,ip)**2*r*r*alo(ip)*sim
    1 r=r+h
      if(k.eq.1)goto 225
      r=r-h1
      nk=n-n0+1
      h=h2
      k=1
      goto 5
  225 continue
      et=0.
      een=0.
      do 58 it=1,ln
      een=een+uvv(it)
      et=et+alo(it)*en(it)
      bl=bl+alo(it)
        if(it.le.l(1))blp=blp+alo(it)
c      sk=sk+skr(it)*alo(it)     !!!???
c      skr(it)=sqrt(skr(it))    !!!???
   58 continue
cccccccccccTAILccccccccccccccccccccccccc28.2.95
		do 592 ip=1,ln
		amas=const(1)
		if(ip.gt.l(1))amas=const(2)
		if(ip.eq.ln.and.l(3).ne.0)amas=const2
		cap=sqrt(-en(ip)/amas)
		rmax=(n0-1)*h1+(n-n0)*h2
		tail=rf(n,ip)**2*(.25/cap**3+rmax/2./cap**2+rmax**2/2./
     *          cap)
		if(ip.le.l(1))sk=sk+(skr(ip)+tail)*alo(ip)
ccccccccccccccccccc charge rad
		sk2=sk2+(skr(ip)+tail)*alo(ip)
		if(l(3).ne.0.and.ip.lt.ln)sk11=sk11+(skr(ip)+tail)*
     *          alo(ip)
592		skr(ip)=sqrt(skr(ip)+tail)
c      call si1(hg,hh,tt,tp,tp11,sk1)
		hh=hg1
		tt=hg2
		tp=hg3
		tp11=hg4
		sk1=hg5
		hh1=hg6
		tc=hg7
      e1=tt*4.*3.141593/5.06769e-3
      e1=e1-REAL(CP)**2/REAL(GP)-REAL(CN)**2/REAL(GN)
      tt=tp
      e2=(.5*et+tt*4.*3.141593)/5.06769e-3
      e2=e2-REAL(CP)**2/REAL(GP)-REAL(CN)**2/REAL(GN)
      e22=(.5*et+tp11*4.*3.141593)/5.06769e-3
      e3=hh*4.*3.141593/5.06769e-3
      e3=e3-REAL(CP)**2/REAL(GP)-REAL(CN)**2/REAL(GN)
		tcmn=hg7*cons*4.*3.141593/5.06769e-3
      sk=sqrt(sk/blp)
ccccccccccccccccccccccccccore rad
		sk2=sqrt(sk2/bl)
      if(l(3).ne.0)bl=bl-alo(ln)
      sk1=sqrt(sk1/bl)
		sk11=sqrt(sk11/bl)
      write(*,55)e1,tcmn
		write(1,55)e1,tcmn
   55 format(4x,'**energy1',e15.7,'tcmn=',e15.7)
        write(*,556)e2, e3
		write(1,556)e2,e3
  556 format(4x,'**en2',e15.7,'en3=',e15.7)
      write(*,555)sk,sk2
		write(1,555)sk,sk2
  555 format(' Charge rad',e12.5,2x,' Total radius',e12.5)
      write(*,56)sk1,sk11
		write(1,56)sk1,sk11
   56 format(4x,'nucl rad',e15.5,2x,'nucl radLN',e12.5)
      return
      end
c
      subroutine si1(hg,hh,tt,tp,tp11,sk1)
      common/param/h1,h2,ht,rt,n,n0,n1
      dimension hg(800,5),pck(5)
      b=(n0-1)*h1+(n-n0)*h2
      n2=n0-2
      k1=1
      h=h1
      r=0.
      do 687 j=1,5
  687 pck(j)=0.
   55 do 685 j=1,5
      pc=0.
      do 61 jl=k1,n2,2
      r=r+2.*h
      rc=hg(jl,j)/6.+2./3.*hg(jl+1,j)+hg(jl+2,j)/6.
   61 pc=pc+rc
  685 pck(j)=pck(j)+pc*h*2.
      if(h.ge.h2)goto 53
      k1=n0
      n2=n-2
      h=h2
      nn2=n-n0-1
      if(nn2/2*2.eq.nn2)n2=n-3
      go to 55
   53 hh=pck(1)
      tt=pck(2)
      tp=pck(3)
      tp11=pck(4)
      sk1=pck(5)
      return
      end
c
      subroutine output(irf1)
      common/rf/rf(800,40)/en/en(40)
      common/param/h1,h2,ht,rt,n,n0,n1
      common/lwf/l(3),alo(40),ln
      common/rr/rr1(800,6,3),rr2(800,6,3)
      common/aj/aj(40)/al/al(40)/skr/skr(40),sk
		common/npri1/npri1(40)
		common/v/uvh(40),uvb(40),upp(40)
		dimension upt(800,40)
		dimension rp(6,4),uv(40)
      write(*,121)
		write(1,121)
  121 format(6x,'n',3x,'l',5x,'j',5x,'E',10x,'skr',13x,'alo')
      do 1 i=1,ln
      write(*,122)npri1(i),al(i),aj(i),en(i),skr(i),alo(i)
		write(1,122)npri1(i),al(i),aj(i),en(i),skr(i),alo(i)
    1 continue
  122 format(2x,i2,2x,f3.0,2x,f3.1,2x,f10.5,2x,e14.5,4x,f10.5)
      if(irf1.eq.0)return
c      write(*,123)
		write(2,123)
  123 format(4x,'RADIAL FUNCTIONS')
c      write(*,124)(al(i),aj(i),i=1,6)
		write(2,124)(al(i),aj(i),i=1,ln)
		write(3,124)(al(i),aj(i),i=1,ln)
  124 format(2x,14x,6('l',f3.0,1x,'j',f4.1,2x))
      r=0.0
      h=h1
      k=0
      nk=n0
   51 contiNUE
      DO 29 in=1,nk
      i=in+n0*k
c      write(*,125)r,(rf(i,j),j=1,6)
		write(2,125)r,(rf(i,j),j=1,ln)
  125 format(2x,7(e12.4))
		if(i.eq.1)goto 29
		do 111 j1=1,6
		do 111 j2=1,3
		rp(j1,j2)=rr1(i,j1,j2)
111		continue
		call ql(i,r,rp,uv)
		do 112 ji=1,ln
112		upt(i,ji)=upp(ji)/5.06769e-3
		if(i.eq.1)r=0.
   29 r=r+h
      if(k.eq.1)goto 52
      r=r-h+h2
      h=h2
      nk=n-n0
      k=1
      go to 51
c   52 write(*,126)(rf(i,ln),i=1,n)
52		write(2,126)(rf(i,ln),i=1,n)
  126 format(2x,7(e12.4))
c      write(*,423)
		write(2,423)
  423 format(4x,'NUCL.DENS.')
		write(3,424)
424		format(5x,'potentials')
		write(4,425)
425		format(5x,'Neutron  densities')
      r=0.
      h=h1
      k=0
      nk=n0
  751 continue
      do 729 in=1,nk
      i=in+n0*k
      rrp=rr1(i,1,1)+rr1(i,1,2)
cccccccccccccccccccccccccccccccccccccccccc28.2.95
		rrnc=0.
		l1=l(2)-1
		do 666 ji=1,l1
		jii=ji+l(1)
666		rrnc=rrnc+alo(jii)*rf(i,jii)**2
		jl=l(1)+l(2)
		v=4.*3.141593*r*r
		if(i.eq.1)goto 355
		rrh=alo(jl)*rf(i,jl)**2/v
		rrnc=rrnc/v
		rcor=rrnc+rr1(i,1,1)
cccccccccccccccccccccccccccccccccccccccccc dens of neutr halo
		write(4,725)r,rrnc,rrh,rr1(i,1,1),rcor
355		write(2,725)r,rr1(i,1,1),rr1(i,1,2),rrp
C		write(3,126)r,(upt(i,j),j=1,ln)
                write(3,126)r,upt(i,1),upt(i,l(1)+1)
  725 format(2x,7(e12.4))
  729 r=r+h
      if(k.eq.1)goto 752
      r=r-h+h2
      h=h2
      nk=n-n0
      k=1
      goto 751
  752 return
      end
c

      SUBROUTINE CLAM(MIN,MAX,RN,G,IQP1,IQP2,C,RLAM,ENBCS,aal,aaj,een,
     *RLAM1)
      IMPLICIT REAL*8 (A-H,O-Z)
!      COMMON/EINDE/E(100),INDE(100)
      CHARACTER IZZZ*1
      dimension :: aal(40), aaj(40), een(40)

 6000 FORMAT(5X,13H*****INITIAL ,A1,
     * 36H VALUE FOR GROUND STATE IS NOT FOUND)
 6001 FORMAT(5X,32H*****C AND LAM ARE NOT FOUND C =,E15.6,10X,5HLAM =,
     *E15.6)

      CINIT=C
      RLINIT=RLAM

!      do I=MIN,MAX
!      print*, aal(I), aaj(I), een(I)
!      end do
!      pause

  200 C1=0
      C2=0
      DO 1 J=MIN,MAX
      RJI=aaj(J)
      C2=C2+(2.D0*RJI+1.D0)
    1 CONTINUE
      C2=C2*G/4.D0

      DO 3 IT=1,30
      C=(C1+C2)/2.D0
      IF(DABS(C1-C2).LE.0.1D0)GOTO100
  101 F=-4.D0/G
      CP2=C*C
      DO 2 J=MIN,MAX
      IF(J.EQ.IQP1.OR.J.EQ.IQP2)GOTO2
  150 CALL CLAMH(J,C,RLAM,RJI,EPSJ,EL,VJ2,aal,aaj,een)
      F=F+(2.D0*RJI+1.D0)/EPSJ
!      print*, RJI, (2.D0*RJI+1.D0)/EPSJ, F; pause
    2 CONTINUE
  160 IF(F)102,102,103
  102 C2=C
      GO TO 3
  103 C1=C
    3 CONTINUE

      IZZZ='C'
  165 WRITE(16,6000)IZZZ
      ENBCS=7777.D0
      GO TO 120

  100 DO 4 IT=1,30
      B1=-4.D0/G
      B2=-RN
      CP2=C*C
      ENBCS=-CP2/G
      A11T=0.D0
      A12=0.D0
      DO 5 J=MIN,MAX
      IF(J.EQ.IQP1.OR.J.EQ.IQP2)GOTO5
  105 CALL CLAMH(J,C,RLAM,RJI,EPSJ,EL,VJ2,aal,aaj,een)
      RJIS=2.D0*RJI+1.D0
      B1=B1+RJIS/EPSJ
      B2=B2+RJIS*VJ2
      A11T=A11T+RJIS/EPSJ**3
      A12=A12+RJIS*EL/EPSJ**3
      ENBCS=ENBCS+RJIS*een(J)*VJ2
    5 CONTINUE

  106 DET1=CP2*A11T*A11T+A12*A12
      DET=-0.5D0*C*DET1
      B1=-B1
      B2=-B2
      IF(DABS(B1)+DABS(B2).LE.1.D-07)GOTO120
  180 DELTAC=(0.5D0*CP2*B1*A11T-B2*A12)/DET
      DELTAL=-C*(A11T*B2+0.5D0*A12*B1)/DET
      C=C+DELTAC
      RLAM=RLAM+DELTAL
      IF(.NOT.(DABS(C).LE.0.01D0))GOTO4
  181 C=0.D0
      GO TO 120
    4 CONTINUE
      WRITE(16,6001)C,RLAM
      ENBCS=0.D0

  120 IF (C.eq.0.D0) THEN
      RLAM = RLAM1
      END IF
      RETURN
      END

      SUBROUTINE CLAMODD(MIN,MAX,RN,G,IQP1,IQP2,C,RLAM,ENBCS,aal,aaj,
     *een,RLAM1,oddl)
      IMPLICIT REAL*8 (A-H,O-Z)
!      COMMON/EINDE/E(100),INDE(100)
      CHARACTER IZZZ*1
      dimension :: aal(40), aaj(40), een(40)
      integer :: oddl

 6000 FORMAT(5X,13H*****INITIAL ,A1,
     * 36H VALUE FOR GROUND STATE IS NOT FOUND)
 6001 FORMAT(5X,32H*****C AND LAM ARE NOT FOUND C =,E15.6,10X,5HLAM =,
     *E15.6)

      CINIT=C
      RLINIT=RLAM

!      do I=MIN,MAX
!      print*, aal(I), aaj(I), een(I)
!      end do
!      pause

  200 C1=0
      C2=0
      DO 1 J=MIN,MAX
      RJI=aaj(J)
      C2=C2+(2.D0*RJI+1.D0)
    1 CONTINUE
      C2=C2*G/4.D0

      DO 3 IT=1,30
      C=(C1+C2)/2.D0
      IF(DABS(C1-C2).LE.0.1D0)GOTO100
  101 F=-4.D0/G
      CP2=C*C
      DO 2 J=MIN,MAX
      IF(J.EQ.IQP1.OR.J.EQ.IQP2)GOTO2
  150 CALL CLAMH(J,C,RLAM,RJI,EPSJ,EL,VJ2,aal,aaj,een)
      F=F+(2.D0*RJI+1.D0)/EPSJ
!      print*, RJI, (2.D0*RJI+1.D0)/EPSJ, F; pause
    2 CONTINUE
  160 IF(F)102,102,103
  102 C2=C
      GO TO 3
  103 C1=C
    3 CONTINUE

      IZZZ='C'
  165 WRITE(16,6000)IZZZ
      ENBCS=7777.D0
      GO TO 120

  100 DO 4 IT=1,30
      B1=-4.D0/G
      B2=-RN
      CP2=C*C
      ENBCS=-CP2/G
      A11T=0.D0
      A12=0.D0
      DO 5 J=MIN,MAX
      IF(J.EQ.IQP1.OR.J.EQ.IQP2)GOTO5
  105 CALL CLAMH(J,C,RLAM,RJI,EPSJ,EL,VJ2,aal,aaj,een)
      RJIS=2.D0*RJI+1.D0
      B1=B1+RJIS/EPSJ
      B2=B2+RJIS*VJ2
      A11T=A11T+RJIS/EPSJ**3
      A12=A12+RJIS*EL/EPSJ**3
      ENBCS=ENBCS+RJIS*een(J)*VJ2
    5 CONTINUE
      CALL CLAMH(oddl,C,RLAM,RJI,EPSJ,EL,VJ2,aal,aaj,een) !odd level
      B1=B1-2.D0/EPSJ
      B2=B2-2.D0*VJ2
      A11T=A11T-2.D0/EPSJ**3
      A12=A12-2.D0*EL/EPSJ**3
!      print*,oddl,RJI,EPSJ,EL,VJ2; pause

  106 DET1=CP2*A11T*A11T+A12*A12
      DET=-0.5D0*C*DET1
      B1=-B1
      B2=-B2
      IF(DABS(B1)+DABS(B2).LE.1.D-07)GOTO120
  180 DELTAC=(0.5D0*CP2*B1*A11T-B2*A12)/DET
      DELTAL=-C*(A11T*B2+0.5D0*A12*B1)/DET
      C=C+DELTAC
      RLAM=RLAM+DELTAL
      IF(.NOT.(DABS(C).LE.0.01D0))GOTO4
  181 C=0.D0
      GO TO 120
    4 CONTINUE
      WRITE(16,6001)C,RLAM
      ENBCS=0.D0

  120 IF (C.eq.0.D0) THEN
      RLAM = RLAM1
      END IF
      RETURN
      END


C***********************************************************************
      SUBROUTINE CLAMH(I,C,RLAM,RJI,EPSJ,EL,VJ2,aal,aaj,een)
      IMPLICIT REAL*8 (A-H,O-Z)

!      COMMON/EINDE/E(100),INDE(100)
!      COMMON/spsv/aal(40),aaj(40),een(40)

      dimension :: aal(40), aaj(40), een(40)

!      print*, I, C, RLAM, RJI, EPSJ, EL, VJ2; pause
!      print*, aal(I), aaj(I), een(I); pause

      RJI=aaj(I)
      EL=een(I)-RLAM
      EPSJ=DSQRT(C*C+EL*EL)
      VJ2=0.5D0*(1.D0-EL/EPSJ)

!      print*, RJI, EL, EPSJ, VJ2; pause

      RETURN
      END
C***********************************************************************
      SUBROUTINE EPSUVS(CP,CN,RLAMP,RLAMN,KEP,KEN,aal,aaj,een,eps,UU,VV)
      IMPLICIT REAL*8 (A-H,O-Z)
      dimension :: aal(40), aaj(40), een(40), eps(40),UU(40),VV(40)

!      COMMON/EINDE/E(100),INDE(100)
!      COMMON/spsv/aal(40),aaj(40),een(40)
!      COMMON/EPSUV/EPS(100),U(100),V(100)

      DO 1 K=1,2
      J=K-1
      C=CP*(1-J)+CN*J
      RLAM=RLAMP*(1-J)+RLAMN*J
      MIN=1+J*KEP
      MAX=KEN*J+KEP
      DO 1 I=MIN,MAX
      EL=een(I)-RLAM
!      IF (C.eq.0.D0) THEN
!      EPS(I)=DABS(EL)
!      ELSE
      EPS(I)=DSQRT(C*C+EL*EL)
!      END IF
      VV(I)=DSQRT((1.D0-EL/EPS(I))/2.D0)
      print*, 23, I, een(I),RLAM, een(I)-RLAM, 1-(een(I)-RLAM)/EPS(I)
      IF(.NOT.(VV(I).LE.0.05D0))GOTO3
  2   VV(I)=0.D0
      UU(I)=1.0D0
      GO TO 1
  3   UU(I)=DSQRT(1.D0-VV(I)*VV(I))
  1   CONTINUE
      RETURN
      END

      block data
      common/en/en(40)
      common/param/h1,h2,ht,rt,n,n0,n1
      common/sl/const(2),cons,const2
      common/aj/aj(40)/al/al(40)/lwf/l(3),alo(40),ln
      common/tsk/t(16),tl(15)
      common/iprint/iter,irf,irf1
      common/ass/pmn(2),pml,a
      common/bf/bf(40)
c     abcde
      common/parname/paramet,hypparamet

      character(len = 20) :: paramet,hypparamet

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   /en/en(19) - single particles energies first approximation (protons, neutons, L-hyperon, 0)
C   /param/h1,h2,ht,rt,n,n0,n1 - calculation net parameters (may differ for light and heavy nuclei):
C   n - number of points,
C   h1 - step in interior part; h2 - step in exterior part; ht, rt - step and radius of speed-up
ccc for light hypernuclei (input in subroutine put1(epsil))
C      r0=8.1
C      r1=2.1
C             for heavy
C      r0=5.6
C      r1=3.5
C       epsil=1.e-4 - accuracy  (input in subroutine put1(epsil))
C   /sl/const(2),cons,const2
C   /aj/aj(19) - single particle orbital momentum
C   /al/al(19) - single particle orbital momentum
C   /lwf/l(3),alo(19),ln - ln - number of single particle states;
C   l(1) - number of proton single particle states,
C   l(2) - number of neutron single particle states, l(3) - for L-hyperon (0 or 1)
C   alo(19) - occupation numbers (as in simple Shell Model)
C   /tsk/t(16),tl(15) - Skyrme interaction parameters t(13) - NN int; tl(15) - LN int.
C   /iprint/iter,irf,irf1
C   /ass/pmn(2),pml,a - proton, neutron and L-hyperon masses (for pure nucleus pml=0.0 !!!)
C   /bf/bf(19) - oscill. parameters for wave function first approximation
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      data iter,irf,irf1/1,2,1/
c      data h1,h2,n,ht,rt/.04,.3,200,.025,.52/

ccccccccccc H5  ccccc cccccccccccccccccccccccccccccc
c      Data ln/3/,l/1,2,0/,alo/1,2,2,0,0,0,7*0,6*0/
c      data al/0.,0.,1.,0.,0.,0.,7*0.,6*0./
c      data aj/.5,.5,1.5,0.0,0.,0.,0.,6*0.,6*0./
c      data en/-27.,-10.,-0.5,0.,0.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.62,1.70,1.73,1.7,1.72,1.73,13*0./

ccccccccccc H6L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/4/,l/1,2,1/,alo/1,2,2,1,0,0,7*0,6*0/
c      data al/0.,0.,1.,0.,0.,0.,7*0.,6*0./
c      data aj/.5,.5,1.5,0.5,0.,0.,0.,6*0.,6*0./
c      data en/-29.,-13.,-3.2,-2.,0.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.62,1.70,1.73,1.7,1.72,1.73,13*0./

ccccccccccc He4L  ccccc cccccccccccccccccccccccccccccc
C      Data ln/3/,l/1,1,1/,alo/2,1,1,0,0,0,7*0,6*0/
C      data al/0.,0.,0.,0.,0.,0.,7*0.,6*0./
C      data aj/.5,.5,.5,0.,0.,0.,0.,6*0.,6*0./
C      data en/-21.,-22.,-9.2,0.,0.,0.,7*0.,6*0./
C      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
C	data bf/1.67,1.71,1.73,1.7,1.72,1.73,13*0./

ccccccccccc He4  ccccc cccccccccccccccccccccccccccccc
c      Data ln/2/,l/1,1,0/,alo/2,2,0,0,0,0,7*0,27*0/
c      data al/0.,0.,0.,0.,0.,0.,7*0.,27*0./
c      data aj/.5,.5,0.,0.0,0.,0.,0.,6*0.,27*0./
c      data en/-15.,-15.,0.,0.,0.,0.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,0.,0.,0.,0.,34*0./

ccccccccccc He8L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/4/,l/1,2,1/,alo/2,2,3,1,0,0,7*0,6*0/
c      data al/0.,0.,1.,0.,0.,0.,7*0.,6*0./
c      data aj/.5,.5,1.5,0.5,0.,0.,0.,6*0.,6*0./
c      data en/-21.,-22.,-9.2,-8.,0.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.73,1.7,1.72,1.73,13*0./

ccccccccccc He8  ccccc cccccccccccccccccccccccccccccc
c      Data ln/3/,l/1,2,0/,alo/2,2,4,0,0,0,7*0,27*0/
c      data al/0.,0.,1.,0.,0.,0.,7*0.,27*0./
c      data aj/.5,.5,1.5,0.0,0.,0.,0.,6*0.,27*0./
c      data en/-25.,-22.,-7.,0.,0.,0.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.72,1.73,34*0./
ccccccccccc He9L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/4/,l/1,2,1/,alo/2,2,4,1,0,0,7*0,27*0/
c      data al/0.,0.,1.,0.,0.,0.,7*0.,27*0./
c      data aj/.5,.5,1.5,0.5,0.,0.,0.,6*0.,27*0./
c      data en/-34.,-21.,-5,-9.,0.,0.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.72,1.73,34*0./

ccccccccccc He9  ccccc cccccccccccccccccccccccccccccc
c      Data ln/4/,l/1,3,0/,alo/2,2,4,2,0,0,7*0,6*0/
c      data al/0.,0.,1.,1.,0.,0.,7*0.,6*0./
c      data aj/.5,.5,1.5,.5,0.,0.,0.,6*0.,6*0./
c      data en/-21.,-22.,-9.2,-7.,0.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,0.,0.,13*0./
ccccccccccc He10L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/5/,l/1,3,1/,alo/2,2,4,1,1,0,7*0,6*0/
c      data al/0.,0.,1.,1.,0.,0.,7*0.,6*0./
c      data aj/.5,.5,1.5,.5,.5,0.,0.,6*0.,6*0./
c      data en/-36.,-21.,-7,-5.,-10.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,0.,13*0./

ccccccccccc He10  ccccc cccccccccccccccccccccccccccccc
c      Data ln/4/,l/1,3,0/,alo/2,2,4,2,0,0,7*0,6*0/
c      data al/0.,0.,1.,1.,0.,0.,7*0.,6*0./
c      data aj/.5,.5,1.5,.5,0.,0.,0.,6*0.,6*0./
c      data en/-21.,-22.,-9.2,-7.,0.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,0.,0.,13*0./
ccccccccccc He11L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/5/,l/1,3,1/,alo/2,2,4,2,1,0,7*0,6*0/
c      data al/0.,0.,1.,1.,0.,0.,7*0.,6*0./
c      data aj/.5,.5,1.5,.5,.5,0.,0.,6*0.,6*0./
c      data en/-21.,-22.,-9.2,-7.,-1.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,0.,13*0./

ccccccccccc Li6  ccccc cccccccccccccccccccccccccccccc
c      Data ln/4/,l/2,2,0/,alo/2,1,2,1,0,0,7*0,6*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.,0.,0.,6*0.,6*0./
c      data en/-33.,-17.9,-26.2,-10.,0.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.73,1.7,1.72,1.73,13*0./

ccccccccccc Li7L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/5/,l/2,2,1/,alo/2,1,2,1,1,0,7*0,6*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,0.,0.,6*0.,6*0./
c      data en/-21.,-7.9,-22.2,-8.,-5.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.73,1.7,1.72,1.73,13*0./

ccccccccccc Li8  ccccc cccccccccccccccccccccccccccccc
c      Data ln/4/,l/2,2,0/,alo/2,1,2,3,0,0,7*0,6*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.,0.,0.,6*0.,6*0./
c      data en/-33.,-17.9,-26.2,-10.,0.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.73,1.7,1.72,1.73,13*0./
ccccccccccc Li9L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/5/,l/2,2,1/,alo/2,1,2,3,1,0,7*0,27*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,27*0./
c      data aj/.5,1.5,.5,1.5,0.5,0.,0.,6*0.,27*0./
c      data en/-33.,-17.9,-26.2,-10.,-10.,0.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.72,1.73,34*0./

ccccccccccc Li9  ccccc cccccccccccccccccccccccccccccc
c      Data ln/4/,l/2,2,0/,alo/2,1,2,4,0,0,7*0,6*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.,0.,0.,6*0.,6*0./
c      data en/-33.,-17.9,-26.2,-10.,0.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.73,1.7,1.72,1.73,13*0./
ccccccccccc Li10L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/5/,l/2,2,1/,alo/2,1,2,4,1,0,7*0,6*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,0.,0.,6*0.,6*0./
c      data en/-21.,-7.9,-22.2,-8.,-5.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.73,1.7,1.72,1.73,13*0./

ccccccccccc Li10  ccccc cccccccccccccccccccccccccccccc
c      Data ln/5/,l/2,3,0/,alo/2,1,2,4,1,0,7*0,6*0/
c      data al/0.,1.,0.,1.,1.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,0.,0.,6*0.,6*0./
c      data en/-33.,-17.9,-26.2,-10.,-5.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.73,1.7,1.72,1.73,13*0./
ccccccccccc Li11L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/6/,l/2,3,1/,alo/2,1,2,4,1,1,7*0,6*0/
c      data al/0.,1.,0.,1.,1.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,0.5,0.,6*0.,6*0./
c      data en/-33.,-17.9,-26.2,-10.,-5.,-10.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.73,1.7,1.72,1.73,13*0./

ccccccccccc Li11  ccccc cccccccccccccccccccccccccccccc
c      Data ln/5/,l/2,3,0/,alo/2,1,2,4,2,0,7*0,6*0/
c      data al/0.,1.,0.,1.,1.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,0.,0.,6*0.,6*0./
c      data en/-33.,-17.9,-26.2,-10.,-5.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.73,1.7,1.72,0.,13*0./
ccccccccccc Li12L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/6/,l/2,3,1/,alo/2,1,2,4,2,1,7*0,6*0/
c      data al/0.,1.,0.,1.,1.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,0.5,7*0.,6*0./
c      data en/-33.,-17.9,-26.2,-10.,-5.,-1.,-10.,6*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.73,1.7,1.72,1.7,0.,12*0./

ccccccccccc Li12  ccccc cccccccccccccccccccccccccccccc
c      Data ln/6/,l/2,4,0/,alo/2,1,2,4,2,1,7*0,6*0/
c      data al/0.,1.,0.,1.,1.,2.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,2.5,0.,6*0.,6*0./
c      data en/-33.,-17.9,-26.2,-10.,-5.,-1.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.73,1.7,1.72,1.73,13*0./
ccccccccccc Li13L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/7/,l/2,4,1/,alo/2,1,2,4,2,1,1,6*0,6*0/
c      data al/0.,1.,0.,1.,1.,2.,0.,6*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,2.5,0.5,6*0.,6*0./
c      data en/-33.,-17.9,-26.2,-10.,-5.,-1.,-10.,6*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.73,1.7,1.72,1.73,1.7,12*0./

ccccccccccc Be7  ccccc cccccccccccccccccccccccccccccc
c      Data ln/4/,l/2,2,0/,alo/2,2,2,1,0,0,7*0,27*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,27*0./
c      data aj/.5,1.5,.5,1.5,0.,0.,0.,6*0.,27*0./
c      data en/-25.,-5.,-26.8,-11.,0.,0.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,0,0,34*0./
ccccccccccc Be8L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/5/,l/2,2,1/,alo/2,2,2,1,1,0,7*0,27*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,27*0./
c      data aj/.5,1.5,.5,1.5,.5,0.,0.,6*0.,27*0./
c      data en/-25.,-5.,-26.8,-11.,-9.,0.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,0,34*0./

ccccccccccc Be8  ccccc cccccccccccccccccccccccccccccc
c      Data ln/4/,l/2,2,0/,alo/2,2,2,2,0,0,7*0,27*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,27*0./
c      data aj/.5,1.5,.5,1.5,0.,0.,0.,6*0.,27*0./
c      data en/-25.,-10.,-26.8,-11.,0.,0.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,0,0,34*0./
ccccccccccc  ccccc cccccccccccccccccccccccccccccc
c      Data ln/5/,l/2,2,1/,alo/2,2,2,2,1,0,7*0,6*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,.5,0.,0.,6*0.,6*0./
c      data en/-33.,-17.9,-26.2,-10.,-1.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,0,13*0./

ccccccccccc Be9  ccccc cccccccccccccccccccccccccccccc
c      Data ln/4/,l/2,2,0/,alo/2,2,2,3,0,0,7*0,6*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.,0.,0.,6*0.,6*0./
c      data en/-33.,-17.9,-26.2,-10.,0.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.73,1.7,1.72,1.73,13*0./
ccccccccccc Be10L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/5/,l/2,2,1/,alo/2,2,2,3,1,0,7*0,6*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,.5,0.,0.,6*0.,6*0./
c      data en/-33.,-17.9,-26.2,-10.,-10.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,0.,13*0./

ccccccccccc Be10  ccccc cccccccccccccccccccccccccccccc
c      Data ln/4/,l/2,2,0/,alo/2,2,2,4,0,0,7*0,6*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.,0.,0.,6*0.,6*0./
c      data en/-33.,-17.9,-26.2,-10.,0.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,0.,0.,13*0./
ccccccccccc Be11L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/5/,l/2,2,1/,alo/2,2,2,4,1,0,7*0,6*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,.5,0.,0.,6*0.,6*0./
c      data en/-33.,-17.9,-26.2,-10.,-10.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,0.,13*0./

ccccccccccc Be11  ccccc cccccccccccccccccccccccccccccc
c      Data ln/5/,l/2,3,0/,alo/2,2,2,4,1,0,7*0,27*0/
c      data al/0.,1.,0.,1.,1.,0.,7*0.,27*0./
c      data aj/.5,1.5,.5,1.5,.5,0.,0.,6*0.,27*0./
c      data en/-35.,-17.,-26.2,-11.,-1.,0.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,0.,34*0./
ccccccccccc Be11 2s_1/2 ccccc ccccccccccccccccccccccccc
c      Data ln/5/,l/2,3,0/,alo/2,2,2,4,1,0,7*0,27*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,27*0./
c      data aj/.5,1.5,.5,1.5,.5,0.,0.,6*0.,27*0./
c      data en/-35.,-17.,-26.2,-11.,-1.,0.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,0.,34*0./
ccccccccccc Be12L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/6/,l/2,3,1/,alo/2,2,2,4,1,1,7*0,6*0/
c      data al/0.,1.,0.,1.,1.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,.5,.5,0.,6*0.,6*0./
c      data en/-36.,-21.,-28.,-12.,-5.,-12.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,1.7,13*0./

ccccccccccc Be12  ccccc cccccccccccccccccccccccccccccc
c      Data ln/5/,l/2,3,0/,alo/2,2,2,4,2,0,7*0,27*0/
c      data al/0.,1.,0.,1.,1.,0.,7*0.,27*0./
c      data aj/.5,1.5,.5,1.5,.5,0.,0.,6*0.,27*0./
c      data en/-35.,-20.9,-26.2,-11.,-5.,0.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,0.,34*0./
ccccccccccc Be13L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/6/,l/2,3,1/,alo/2,2,2,4,2,1,7*0,6*0/
c      data al/0.,1.,0.,1.,1.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,.5,.5,0.,6*0.,6*0./
c      data en/-36.,-21.,-28.,-12.,-5.,-12.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,1.7,13*0./

ccccccccccc Be13  ccccc cccccccccccccccccccccccccccccc
c      Data ln/6/,l/2,4,0/,alo/2,2,2,4,2,1,7*0,6*0/
c      data al/0.,1.,0.,1.,1.,2.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,.5,2.5,0.,6*0.,6*0./
c      data en/-35.,-20.9,-26.2,-11.,-5.,-3.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,1.7,13*0./
ccccccccccc Be14L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/7/,l/2,4,1/,alo/2,2,2,4,2,1,1,6*0,6*0/
c      data al/0.,1.,0.,1.,1.,2.,0.,6*0.,6*0./
c      data aj/.5,1.5,.5,1.5,.5,2.5,.5,6*0.,6*0./
c      data en/-39.,-24.,-28.,-12.,-5.,-2.,-13.,6*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,1.7,1.7,12*0./

ccccccccccc Be14  ccccc cccccccccccccccccccccccccccccc
c      Data ln/6/,l/2,4,0/,alo/2,2,2,4,2,2,7*0,6*0/
c      data al/0.,1.,0.,1.,1.,2.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,.5,2.5,0.,6*0.,6*0./
c      data en/-35.,-20.9,-26.2,-11.,-5.,-3.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,1.7,13*0./
ccccccccccc Be15L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/7/,l/2,4,1/,alo/2,2,2,4,2,2,1,6*0,6*0/
c      data al/0.,1.,0.,1.,1.,2.,0.,6*0.,6*0./
c      data aj/.5,1.5,.5,1.5,.5,2.5,.5,6*0.,6*0./
c      data en/-39.,-24.,-28.,-12.,-5.,-2.,-13.,6*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,1.7,1.7,12*0./

ccccccccccc B7  ccccc cccccccccccccccccccccccccccccc
c      Data ln/3/,l/2,1,0/,alo/2,3,2,0,0,0,7*0,27*0/
c      data al/0.,1.,0.,0.,0.,0.,7*0.,27*0./
c      data aj/.5,1.5,.5,0.,0.,0.,0.,6*0.,27*0./
c      data en/-20.,-5.5,-30.,0.,0.,0.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.73,0.,0,0,34*0./
ccccccccccc B8L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/4/,l/2,1,1/,alo/2,3,2,1,0,0,7*0,27*0/
c      data al/0.,1.,0.,0.,0.,0.,7*0.,27*0./
c      data aj/.5,1.5,.5,0.5,0.,0.,0.,6*0.,27*0./
c      data en/-20.,-5.5,-30.,-10.,0.,0.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,0.,0,34*0./

ccccccccccc B8  ccccc cccccccccccccccccccccccccccccc
c      Data ln/4/,l/2,2,0/,alo/2,3,2,1,0,0,7*0,27*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,27*0./
c      data aj/.5,1.5,.5,1.5,0.,0.,0.,6*0.,27*0./
c      data en/-20.,-5.5,-30.,-15.,0.,0.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,0,0,34*0./
ccccccccccc B9L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/5/,l/2,2,1/,alo/2,3,2,1,1,0,7*0,27*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,27*0./
c      data aj/.5,1.5,.5,1.5,0.5,0.,0.,6*0.,27*0./
c      data en/-25.,-10.9,-30.2,-12.,-10.,0.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,0,34*0./

ccccccccccc B9  ccccc cccccccccccccccccccccccccccccc
c      Data ln/4/,l/2,2,0/,alo/2,3,2,2,0,0,7*0,6*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.,0.,0.,6*0.,6*0./
c      data en/-28.,-11.9,-30.2,-14.,0.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,0,0,13*0./
ccccccccccc B10L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/5/,l/2,2,1/,alo/2,3,2,2,1,0,7*0,6*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,.5,0.,0.,6*0.,6*0./
c      data en/-28.,-11.9,-30.2,-14.,-10.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,0,13*0./


ccccccccccc B10  ccccc cccccccccccccccccccccccccccccc
c      Data ln/4/,l/2,2,0/,alo/2,3,2,3,0,0,7*0,27*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,27*0./
c      data aj/.5,1.5,.5,1.5,0.,0.,0.,6*0.,27*0./
c      data en/-28.,-11.9,-30.2,-14.,0.,0.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,0,0,34*0./
ccccccccccc B11L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/5/,l/2,2,1/,alo/2,3,2,3,1,0,7*0,27*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,27*0./
c      data aj/.5,1.5,.5,1.5,.5,0.,0.,6*0.,27*0./
c      data en/-28.,-11.9,-30.2,-14.,-10.,0.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,0,34*0./

ccccccccccc B11  ccccc cccccccccccccccccccccccccccccc
c      Data ln/4/,l/2,2,0/,alo/2,3,2,4,0,0,7*0,6*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.,0.,0.,6*0.,6*0./
c      data en/-28.,-11.9,-30.2,-14.,0.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,0.,0.,13*0./
cccccccccc B12L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/5/,l/2,2,1/,alo/2,3,2,4,1,8*0,6*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,0.,0.,6*0.,6*0./
c      data en/-28.,-11.9,-30.2,-14.,-10.,8*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,0.,13*0./

ccccccccccc B12  ccccc cccccccccccccccccccccccccccccc
c      Data ln/5/,l/2,3,0/,alo/2,3,2,4,1,0,7*0,6*0/
c      data al/0.,1.,0.,1.,1.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,0.,0.,6*0.,6*0./
c      data en/-28.,-11.9,-30.2,-14.,-5.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,0,13*0./
ccccccccccc B13L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/6/,l/2,3,1/,alo/2,3,2,4,1,1,7*0,6*0/
c      data al/0.,1.,0.,1.,1.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,0.5,0.,6*0.,6*0./
c      data en/-28.,-11.9,-30.2,-14.,-5.,-10.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,1.7,13*0./

ccccccccccc B13  ccccc cccccccccccccccccccccccccccccc
c      Data ln/5/,l/2,3,0/,alo/2,3,2,4,2,0,7*0,6*0/
c      data al/0.,1.,0.,1.,1.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,0.,0.,6*0.,6*0./
c      data en/-36.,-21.,-31.,-14.,-7.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,0.,13*0./
ccccccccccc B14L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/6/,l/2,3,1/,alo/2,3,2,4,2,1,7*0,6*0/
c      data al/0.,1.,0.,1.,1.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,0.5,7*0.,6*0./
c      data en/-37.,-22.,-32.,-14.,-8.,-10.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,1.7,0.,12*0./

ccccccccccc B14  ccccc cccccccccccccccccccccccccccccc
c      Data ln/6/,l/2,4,0/,alo/2,3,2,4,2,1,7*0,6*0/
c      data al/0.,1.,0.,1.,1.,2.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,2.5,0.,6*0.,6*0./
c      data en/-36.,-21.,-31.,-14.,-7.,-1.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,1.7,13*0./
ccccccccccc B15L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/7/,l/2,4,1/,alo/2,3,2,4,2,1,1,6*0,6*0/
c      data al/0.,1.,0.,1.,1.,2.,0.,6*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,2.5,0.5,6*0.,6*0./
c      data en/-37.,-22.,-32.,-14.,-8.,-1.,-10.,6*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,1.7,1.7,12*0./

ccccccccccc B15  ccccc cccccccccccccccccccccccccccccc
c      Data ln/6/,l/2,4,0/,alo/2,3,2,4,2,2,7*0,6*0/
c      data al/0.,1.,0.,1.,1.,2.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,2.5,0.,6*0.,6*0./
c      data en/-36.,-21.,-31.,-14.,-7.,-1.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,1.7,13*0./
ccccccccccc B16L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/7/,l/2,4,1/,alo/2,3,2,4,2,2,1,6*0,6*0/
c      data al/0.,1.,0.,1.,1.,2.,0.,6*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,2.5,0.5,6*0.,6*0./
c      data en/-37.,-22.,-32.,-14.,-8.,-1.,-10.,6*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,1.7,1.7,12*0./

ccccccccccc B16  ccccc cccccccccccccccccccccccccccccc
c      Data ln/6/,l/2,4,0/,alo/2,3,2,4,2,3,7*0,6*0/
c      data al/0.,1.,0.,1.,1.,2.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,2.5,0.,6*0.,6*0./
c      data en/-36.,-21.,-31.,-14.,-7.,-1.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,1.7,13*0./
ccccccccccc B17L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/7/,l/2,4,1/,alo/2,3,2,4,2,3,1,6*0,6*0/
c      data al/0.,1.,0.,1.,1.,2.,0.,6*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,2.5,0.5,6*0.,6*0./
c      data en/-37.,-22.,-32.,-14.,-8.,-1.,-10.,6*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,1.7,1.7,12*0./

cccccccccccc  O16  ccccccccccccccccccccccccccccccccccccc
c      Data ln/6/,l/3,3,0/,alo/2,4,2,2,4,2,7*0,27*0/
c      data al/0.,1.,1.,0.,1.,1.,7*0.,27*0./
c      data aj/.5,1.5,.5,.5,1.5,0.5,0.,6*0.,27*0./
c      data en/-29.5,-16.9,-10.2,-32.,-21.,-8.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.72,1.73,34*0./

cccccccccccccc  18O   cccccccccccccccccccccccccccccccccc
c      Data ln/7/,l/3,4,0/,alo/2,4,2,2,4,2,2,6*0,6*0/
c      data al/0.,1.,1.,0.,1.,1.,2.,6*0.,6*0./
c      data aj/.5,1.5,.5,.5,1.5,0.5,2.5,6*0.,6*0./
c      data en/-34.5,-20.9,-15.2,-35.,-20.,-15.,-10.,6*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.72,1.73,1.8,12*0./

cccccccccccccc   18Ne  ccccccccccccccccccccccccccccccccc
c      Data ln/7/,l/4,3,0/,alo/2,4,2,2,2,4,2,6*0,6*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,6*0.,6*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,0.5,6*0.,6*0./
c      data en/-33.5,-18.9,-13.2,-8.,-34.,-25.,-14.,6*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.73,1.75,1.7,1.73,1.8,12*0./

cccccccccccccc  16OL  cccccccccccccccccccccccccccccccccc
c		Data ln/7/,l/3,3,1/,alo/2,4,2,2,3,2,1,6*0,6*0/
c      data al/0.,1.,1.,0.,1.,1.,1.,6*0.,6*0./
c      data aj/.5,1.5,.5,.5,1.5,0.5,.5,6*0.,6*0./
c      data en/-31.5,-16.9,-11.2,-35.,-20.,-14.,-6.,6*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	 data bf/1.67,1.71,1.73,1.7,1.72,1.73,1.67,12*0./

ccccccccccc Be11 ccccccc k=0.7995 for 1p1/2; 1.19 for 2s1/2
c      Data ln/5/,l/2,3,0/,alo/2,2,2,4,1,0,7*0,6*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,6*0./
c	data aj/.5,1.5,0.5,1.5,0.5,0.,0.,6*0.,6*0./
c      data en/-31.,-19.9,-26.2,-10.,-10.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.72,14*0./

ccccccccccc Be12l ccccccc k=0.7995 for 1p1/2; 1.19 for 2s1/2
c      Data ln/6/,l/2,3,1/,alo/2,2,2,4,1,1,7*0,6*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,0.5,0.,6*0.,6*0./
c      data en/-30.5,-14.9,-24.2,-12.,-3.,-9.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/

cccccccccccc c8 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/3/,l/2,1,0/,alo/2,4,2,0,0,0,7*0,27*0/
c      data al/0.,1.,0.,0.,0.,0.,7*0.,27*0./
c      data aj/.5,1.5,.5,0.,0.,0.,0.,6*0.,27*0./
c      data en/-13.2,-0.7,-25.4,0.,0.,0.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.65,1.72,1.7,0.,0.,0.,0.,33*0./                            !1.62,1.7,1.6
cccccccccccc c9L cccccccccccccccccccccccccccccccccccccccc
c      Data ln/4/,l/2,1,1/,alo/2,4,2,1,0,0,7*0,27*0/
c      data al/0.,1.,0.,0.,0.,0.,7*0.,27*0./
c      data aj/.5,1.5,.5,.5,0.,0.,0.,6*0.,27*0./
c      data en/-20.5,-1.9,-32.2,-10.,0.,0.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.62,1.7,1.6,1.7,0.,0.,0.,33*0./

cccccccccccc c9 unocc cccccccccccccccccccccccccccccccccccccccc
c      Data ln/6/,l/3,3,0/,alo/2,4,0,2,1,0,7*0,27*0/
c      data al/0.,1.,1.,0.,1.,1.,7*0.,27*0./
c      data aj/.5,1.5,.5,.5,1.5,.5,7*0.,27*0./
c      data en/-26.5,-8.9,-1.,-34.2,-17.,-1.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.62,1.7,1.7,1.6,1.74,1.74,0.,33*0./
cccccccccccc B8 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/6/,l/3,3,0/,alo/2,3,0,2,1,0,7*0,27*0/
c      data al/0.,1.,1.,0.,1.,1.,7*0.,27*0./
c      data aj/.5,1.5,.5,0.5,1.5,0.5,0.,6*0.,27*0./
c      data en/-26.5,-8.9,-1.,-34.2,-17.,-5.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.71,1.6,1.74,1.75,0.,33*0./
cccccccccccc B8 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/4/,l/2,2,0/,alo/2,3,2,1,0,7*0,28*0/
c      data al/0.,1.,0.,1.,7*0.,29*0./
c      data aj/.5,1.5,0.5,1.5,0.,6*0.,29*0./
c      data en/-26.5,-8.9,-34.2,-17.,7*0.,29*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.6,1.74,0.,35*0./
cccccccccccc c10L cccccccccccccccccccccccccccccccccccccccc
c      Data ln/5/,l/2,2,1/,alo/2,4,2,1,1,0,7*0,27*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,27*0./
c      data aj/.5,1.5,.5,1.5,.5,0.,0.,6*0.,27*0./
c      data en/-26.5,-11.9,-34.2,-17.,-10.,0.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.62,1.7,1.6,1.74,1.7,0.,0.,33*0./

cccccccccccc c10 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/4/,l/2,2,0/,alo/2,4,2,2,0,0,7*0,27*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,27*0./
c      data aj/.5,1.5,.5,1.5,0.,0.,0.,6*0.,27*0./
c      data en/-26.5,-8.9,-34.2,-17.,0.,0.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.62,1.7,1.6,1.74,0.,0.,0.,33*0./
cccccccccccc c11L cccccccccccccccccccccccccccccccccccccccc
c      Data ln/5/,l/2,2,1/,alo/2,4,2,2,1,0,7*0,27*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,27*0./
c      data aj/.5,1.5,.5,1.5,.5,0.,0.,6*0.,27*0./
c      data en/-26.5,-11.9,-34.2,-17.,-10.,0.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.62,1.7,1.6,1.74,1.7,0.,0.,33*0./

cccccccccccc c11 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/4/,l/2,2,0/,alo/2,4,2,3,0,0,7*0,6*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.,0.,0.,6*0.,6*0./
c      data en/-26.5,-11.9,-34.2,-17.,0.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.6,1.74,0.,0.,0.,12*0./
cccccccccccc c12L cccccccccccccccccccccccccccccccccccccccc
c      Data ln/5/,l/2,2,1/,alo/2,4,2,3,1,0,7*0,6*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,0.,0.,6*0.,6*0./
c      data en/-26.5,-11.9,-34.2,-17.,-1.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.6,1.74,1.6,0.,0.,12*0./

cccccccccccc c12 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/4/,l/2,2,0/,alo/2,4,2,4,0,0,7*0,27*0/
c      data al/0.,1.,0.,1.,0.,0.,7*0.,27*0./
c      data aj/.5,1.5,.5,1.5,0.,0.,0.,6*0.,27*0./
c      data en/-26.5,-11.9,-34.2,-17.,0.,0.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.62,1.7,1.6,1.74,0.,0.,0.,33*0./
cccccccccccc c13L cccccccccccccccccccccccccccccccccccccccc
c      Data ln/5/,l/2,2,1/,alo/2,4,2,4,1,0,34*0/
c      data al/0.,1.,0.,1.,0.,0.,34*0./
c      data aj/.5,1.5,.5,1.5,0.5,0.,34*0./
c      data en/-26.5,-11.9,-34.2,-17.,-1.,0.,34*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.62,1.7,1.6,1.74,1.6,0.,34*0./

cccccccccccc c13 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/5/,l/2,3,0/,alo/2,4,2,4,1,0,7*0,6*0/
c      data al/0.,1.,0.,1.,1.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,0.,0.,6*0.,6*0./
c      data en/-26.5,-11.9,-34.2,-17.,-6.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.6,1.74,1.4,0.,0.,12*0./
cccccccccccc c14L cccccccccccccccccccccccccccccccccccccccc
c      Data ln/6/,l/2,3,1/,alo/2,4,2,4,1,1,7*0,27*0/
c      data al/0.,1.,0.,1.,1.,0.,7*0.,27*0./
c      data aj/.5,1.5,.5,1.5,0.5,0.5,0.,6*0.,27*0./
c      data en/-26.5,-11.9,-34.2,-17.,-6.,-1.0,7*0.,27*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.62,1.7,1.6,1.74,1.4,1.5,0.,33*0./

cccccccccccc c14 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/5/,l/2,3,0/,alo/2,4,2,4,2,0,7*0,6*0/
c      data al/0.,1.,0.,1.,1.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,0.,0.,6*0.,6*0./
c      data en/-26.5,-11.9,-34.2,-17.,-6.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.6,1.74,1.4,0.,0.,12*0./
cccccccccccc c15L cccccccccccccccccccccccccccccccccccccccc
c      Data ln/6/,l/2,3,1/,alo/2,4,2,4,2,1,7*0,27*0/
c      data al/0.,1.,0.,1.,1.,0.,7*0.,27*0./
c      data aj/.5,1.5,.5,1.5,0.5,0.5,0.,6*0.,27*0./
c      data en/-26.5,-11.9,-34.2,-17.,-6.,-1.0,7*0.,27*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.6,1.74,1.4,1.5,0.,33*0./

cccccccccccc  C15  ccccccccccccccccccccccccccccccccccccc
c      Data ln/6/,l/2,4,0/,alo/2,4,2,4,2,1,7*0,6*0/
c      data al/0.,1.,0.,1.,1.,2.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,2.5,7*0.,6*0./
c      data en/-29.5,-16.9,-30.2,-22.,-11.,-4.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.72,1.73,13*0./
cccccccccccc  C16L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/7/,l/2,4,1/,alo/2,4,2,4,2,1,1,6*0,6*0/
c      data al/0.,1.,0.,1.,1.,2.,0.,6*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,2.5,0.5,6*0.,6*0./
c      data en/-29.5,-16.9,-30.2,-22.,-11.,-4.,-1.,6*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.72,1.73,1.7,12*0./

cccccccccccc  C16  ccccccccccccccccccccccccccccccccccccc
c      Data ln/6/,l/2,4,0/,alo/2,4,2,4,2,2,7*0,6*0/
c      data al/0.,1.,0.,1.,1.,2.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,2.5,7*0.,6*0./
c      data en/-29.5,-16.9,-30.2,-22.,-11.,-4.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.72,1.73,13*0./
cccccccccccc  C17L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/7/,l/2,4,1/,alo/2,4,2,4,2,2,1,6*0,6*0/
c      data al/0.,1.,0.,1.,1.,2.,0.,6*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,2.5,0.5,6*0.,6*0./
c      data en/-29.5,-16.9,-30.2,-22.,-11.,-4.,-1.,6*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.73,1.7,1.72,1.73,1.7,12*0./

cccccccccccc  C17  ccccccccccccccccccccccccccccccccccccc
c      Data ln/6/,l/2,4,0/,alo/2,4,2,4,2,3,7*0,6*0/
c      data al/0.,1.,0.,1.,1.,2.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,2.5,7*0.,6*0./
c      data en/-29.5,-16.9,-30.2,-22.,-11.,-4.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.72,1.73,13*0./
cccccccccccc  C18L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/7/,l/2,4,1/,alo/2,4,2,4,2,3,1,6*0,6*0/
c      data al/0.,1.,0.,1.,1.,2.,0.,6*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,2.5,0.5,6*0.,6*0./
c      data en/-29.5,-16.9,-30.2,-22.,-11.,-4.,-1.,6*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.73,1.7,1.72,1.73,1.7,12*0./

cccccccccccc  C18  ccccccccccccccccccccccccccccccccccccc
c      Data ln/6/,l/2,4,0/,alo/2,4,2,4,2,4,7*0,6*0/
c      data al/0.,1.,0.,1.,1.,2.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,2.5,7*0.,6*0./
c      data en/-29.5,-16.9,-30.2,-22.,-11.,-4.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.72,1.73,13*0./
cccccccccccc  C19L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/7/,l/2,4,1/,alo/2,4,2,4,2,4,1,6*0,6*0/
c      data al/0.,1.,0.,1.,1.,2.,0.,6*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,2.5,0.5,6*0.,6*0./
c      data en/-29.5,-16.9,-30.2,-22.,-11.,-4.,-1.,6*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.73,1.7,1.72,1.73,1.7,12*0./

cccccccccccc  C19  ccccccccccccccccccccccccccccccccccccc
c      Data ln/6/,l/2,4,0/,alo/2,4,2,4,2,5,7*0,6*0/
c      data al/0.,1.,0.,1.,1.,2.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,2.5,7*0.,6*0./
c      data en/-29.5,-16.9,-30.2,-22.,-11.,-4.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.72,1.73,13*0./
cccccccccccc  C20L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/7/,l/2,4,1/,alo/2,4,2,4,2,5,1,6*0,6*0/
c      data al/0.,1.,0.,1.,1.,2.,0.,6*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,2.5,0.5,6*0.,6*0./
c      data en/-29.5,-16.9,-30.2,-22.,-11.,-4.,-1.,6*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.72,1.73,1.7,12*0./

cccccccccccc  C20  ccccccccccccccccccccccccccccccccccccc
c      Data ln/6/,l/2,4,0/,alo/2,4,2,4,2,6,7*0,6*0/
c      data al/0.,1.,0.,1.,1.,2.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,2.5,7*0.,6*0./
c      data en/-29.5,-16.9,-30.2,-22.,-11.,-4.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.72,1.73,13*0./
cccccccccccc  C21L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/7/,l/2,4,1/,alo/2,4,2,4,2,6,1,6*0,27*0/
c      data al/0.,1.,0.,1.,1.,2.,0.,6*0.,27*0./
c      data aj/.5,1.5,.5,1.5,0.5,2.5,0.5,6*0.,27*0./
c      data en/-29.5,-16.9,-30.2,-22.,-11.,-4.,-1.,6*0.,27*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.72,1.73,1.7,33*0./

cccccccccccc  C21  ccccccccccccccccccccccccccccccccccccc
c      Data ln/7/,l/2,5,0/,alo/2,4,2,4,2,6,1,6*0,6*0/
c      data al/0.,1.,0.,1.,1.,2.,0.,6*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,2.5,0.5,6*0.,6*0./
c      data en/-29.5,-16.9,-30.2,-22.,-11.,-4.,-3.,6*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.72,1.73,1.72,12*0./
cccccccccccc  C22L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/8/,l/2,5,1/,alo/2,4,2,4,2,6,1,1,5*0,6*0/
c      data al/0.,1.,0.,1.,1.,2.,0.,0.,5*0.,6*0./
c      data aj/.5,1.5,.5,1.5,0.5,2.5,0.5,0.5,5*0.,6*0./
c      data en/-29.5,-16.9,-30.2,-22.,-11.,-4.,-3.,-1.,5*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.72,1.73,1.72,1.7,11*0./

cccccccccccc  C22  ccccccccccccccccccccccccccccccccccccc
c      Data ln/7/,l/2,5,0/,alo/2,4,2,4,2,6,2,6*0,27*0/
c      data al/0.,1.,0.,1.,1.,2.,0.,6*0.,27*0./
c      data aj/.5,1.5,.5,1.5,0.5,2.5,0.5,6*0.,27*0./
c      data en/-40.,-30.,-35.,-20.,-15.,-4.,-3.,6*0.,27*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.72,1.73,1.72,33*0./
cccccccccccc  C23L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/8/,l/2,5,1/,alo/2,4,2,4,2,6,2,1,5*0,27*0/
c      data al/0.,1.,0.,1.,1.,2.,0.,0.,5*0.,27*0./
c      data aj/.5,1.5,.5,1.5,0.5,2.5,0.5,0.5,5*0.,27*0./
c      data en/-40.,-30.,-35.,-20.,-15.,-4.,-3.,-15.,5*0.,27*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.72,1.73,1.72,1.7,32*0./

cccccccccccc  C24L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/9/,l/2,6,1/,alo/2,4,2,4,2,6,2,1,1,5*0,26*0/
c      data al/0.,1.,0.,1.,1.,2.,0.,2.,0.,5*0.,26*0./
c      data aj/.5,1.5,.5,1.5,0.5,2.5,0.5,1.5,0.5,5*0.,26*0./
c      data en/-40.,-30.,-35.,-20.,-15.,-4.,-3.,-1.,-15.,5*0.,26*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.72,1.73,1.72,1.72,1.7,31*0./

ccccccccccc N10  ccccc cccccccccccccccccccccccccccccc
c      Data ln/5/,l/3,2,0/,alo/2,4,1,2,1,0,7*0,27*0/
c      data al/0.,1.,1.,0.,1.,0.,7*0.,27*0./
c      data aj/.5,1.5,.5,0.5,1.5,0.,0.,6*0.,27*0./
c      data en/-23.,-7.9,-0.5,-35.,-20.,0.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,0,34*0./
ccccccccccc N11L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/6/,l/3,2,1/,alo/2,4,1,2,1,1,7*0,27*0/
c      data al/0.,1.,1.,0.,1.,0.,7*0.,27*0./
c      data aj/.5,1.5,.5,0.5,1.5,0.5,0.,6*0.,27*0./
c      data en/-20.,-6,-0.3,-35.,-20.,-11.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,1.7,34*0./

ccccccccccc N11  ccccc cccccccccccccccccccccccccccccc
c      Data ln/5/,l/3,2,0/,alo/2,4,1,2,2,0,7*0,27*0/
c      data al/0.,1.,1.,0.,1.,0.,7*0.,27*0./
c      data aj/.5,1.5,.5,0.5,1.5,0.,0.,6*0.,27*0./
c      data en/-27.2,-10.8,-3.9,-35.4,-20.,0.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,0,34*0./
ccccccccccc N12L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/6/,l/3,2,1/,alo/2,4,1,2,2,1,7*0,27*0/
c      data al/0.,1.,1.,0.,1.,0.,7*0.,27*0./
c      data aj/.5,1.5,.5,0.5,1.5,0.5,0.,6*0.,27*0./
c      data en/-27.2,-10.8,-3.9,-35.4,-20.,-10.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,1.7,34*0./

ccccccccccc N12  ccccc cccccccccccccccccccccccccccccc
c      Data ln/5/,l/3,2,0/,alo/2,4,1,2,3,0,7*0,6*0/
c      data al/0.,1.,1.,0.,1.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,0.5,1.5,0.,0.,6*0.,6*0./
c      data en/-27.2,-10.8,-3.9,-35.4,-20.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,0,13*0./
ccccccccccc N13L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/6/,l/3,2,1/,alo/2,4,1,2,3,1,7*0,27*0/
c      data al/0.,1.,1.,0.,1.,0.,7*0.,27*0./
c      data aj/.5,1.5,.5,0.5,1.5,0.5,0.,6*0.,27*0./
c      data en/-30.,-15.9,-3.2,-30.,-20.,-12.,7*0.,27*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,1.7,34*0./

cccccccccccc  N13  ccccccccccccccccccccccccccccccccccccc
c      Data ln/5/,l/3,2,0/,alo/2,4,1,2,4,0,0,12*0/
c      data al/0.,1.,1.,0.,1.,0.,0.,12*0./
c      data aj/0.5,1.5,0.5,0.5,1.5,0.,0.,12*0./
c      data en/-30.5,-15.9,-6.0,-35.2,-22.,0.,0.,12*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.7,1.73,1.7,0.,0.,12*0./
cccccccccccc  N14L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/6/,l/3,2,1/,alo/2,4,1,2,4,1,0,12*0/
c      data al/0.,1.,1.,0.,1.,0.,0.,12*0./
c      data aj/0.5,1.5,0.5,0.5,1.5,0.5,0.,12*0./
c      data en/-30.5,-15.9,-6.0,-35.2,-22.,-10.,0.,12*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.7,1.73,1.7,1.7,0.,12*0./

ccccccccccc N14  ccccc cccccccccccccccccccccccccccccc
c      Data ln/6/,l/3,3,0/,alo/2,4,1,2,4,1,7*0,6*0/
c      data al/0.,1.,1.,0.,1.,1.,7*0.,6*0./
c      data aj/.5,1.5,.5,0.5,1.5,0.5,0.,6*0.,6*0./
c      data en/-31.,-16.,-9.,-35.,-19.,-12.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,1.7,13*0./
ccccccccccc N15L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/7/,l/3,3,1/,alo/2,4,1,2,4,1,1,6*0,6*0/
c      data al/0.,1.,1.,0.,1.,1.,0.,6*0.,6*0./
c      data aj/.5,1.5,.5,0.5,1.5,0.5,0.5,6*0.,6*0./
c      data en/-32.,-16.,-9.,-36.,-20.,-12.,-13.,6*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,1.7,1.7,12*0./

cccccccccccc  N15  ccccccccccccccccccccccccccccccccccccc
c      Data ln/6/,l/3,3,0/,alo/2,4,1,2,4,2,0,12*0/
c      data al/0.,1.,1.,0.,1.,1.,0.,12*0./
c      data aj/0.5,1.5,0.5,0.5,1.5,0.5,0.,12*0./
c      data en/-35.5,-25.9,-15.0,-35.2,-22.,-11.,0.,12*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.7,1.73,1.7,1.72,0.,12*0./
cccccccccccc  N16L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/7/,l/3,3,1/,alo/2,4,1,2,4,2,1,12*0/
c      data al/0.,1.,1.,0.,1.,1.,0.,12*0./
c      data aj/0.5,1.5,0.5,0.5,1.5,0.5,0.5,12*0./
c      data en/-35.5,-25.9,-15.0,-35.2,-22.,-11.,-10.,12*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.7,1.73,1.7,1.72,1.7,12*0./

ccccccccccc N16  ccccc cccccccccccccccccccccccccccccc
c      Data ln/7/,l/3,4,0/,alo/2,4,1,2,4,2,1,6*0,6*0/
c      data al/0.,1.,1.,0.,1.,1.,2.,6*0.,6*0./
c      data aj/.5,1.5,.5,0.5,1.5,0.5,2.5,6*0.,6*0./
c      data en/-31.,-16.,-9.,-35.,-19.,-12.,-5.,6*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,1.7,1.7,12*0./
ccccccccccc N17L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/8/,l/3,4,1/,alo/2,4,1,2,4,2,1,1,5*0,6*0/
c      data al/0.,1.,1.,0.,1.,1.,2.,0.,5*0.,6*0./
c      data aj/.5,1.5,.5,0.5,1.5,0.5,2.5,0.5,5*0.,6*0./
c      data en/-32.,-16.,-9.,-36.,-20.,-12.,-5.,-13.,5*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,1.7,1.7,1.7,11*0./

cccccccccccc  B19  ccccccccccccccccccccccccccccccccccccc
c      Data ln/6/,l/2,4,0/,alo/2,3,2,4,2,6,13*0/
c      data al/0.,1.,0.,1.,1.,2.,13*0./
c      data aj/.5,1.5,.5,1.5,0.5,2.5,13*0./
c      data en/-29.5,-16.9,-30.2,-22.,-11.,-4.,13*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.73,1.7,1.72,1.73,13*0./

cccccccccccc  N21  ccccccccccccccccccccccccccccccccccccc
c      Data ln/7/,l/3,4,0/,alo/2,4,1,2,4,2,6,12*0/
c      data al/0.,1.,1.,0.,1.,1.,2.,12*0./
c      data aj/0.5,1.5,0.5,0.5,1.5,0.5,2.5,12*0./
c      data en/-45.5,-29.9,-15.0,-30.2,-22.,-11.,-4.,12*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.7,1.73,1.7,1.72,1.73,12*0./

cccccccccccc  O12  ccccccccccccccccccccccccccccccccccccc
c      Data ln/5/,l/3,2,0/,alo/2,4,2,2,2,0,0,33*0/
c      data al/0.,1.,1.,0.,1.,0.,0.,33*0./
c      data aj/0.5,1.5,0.5,0.5,1.5,0.,0.,33*0./
c      data en/-30.5,-15.9,-6.0,-35.2,-22.,0.,0.,33*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.7,1.73,1.7,0.,0.,33*0./
cccccccccccc  O13L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/6/,l/3,2,1/,alo/2,4,2,2,2,1,0,33*0/
c      data al/0.,1.,1.,0.,1.,0.,0.,33*0./
c      data aj/0.5,1.5,0.5,0.5,1.5,0.5,0.,33*0./
c      data en/-30.5,-15.9,-6.0,-35.2,-22.,-10.,0.,33*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.7,1.73,1.7,1.7,0.,33*0./

cccccccccccc  O13  ccccccccccccccccccccccccccccccccccccc
c      Data ln/5/,l/3,2,0/,alo/2,4,2,2,3,0,0,12*0/
c      data al/0.,1.,1.,0.,1.,0.,0.,12*0./
c      data aj/0.5,1.5,0.5,0.5,1.5,0.,0.,12*0./
c      data en/-30.5,-15.9,-6.0,-35.2,-22.,0.,0.,12*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.7,1.73,1.7,0.,0.,12*0./
cccccccccccc  O14L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/6/,l/3,2,1/,alo/2,4,2,2,3,1,0,12*0/
c      data al/0.,1.,1.,0.,1.,0.,0.,12*0./
c      data aj/0.5,1.5,0.5,0.5,1.5,0.5,0.,12*0./
c      data en/-30.5,-15.9,-6.0,-35.2,-22.,-10.,0.,12*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.7,1.73,1.7,1.7,0.,12*0./

cccccccccccc  O14  ccccccccccccccccccccccccccccccccccccc
c      Data ln/5/,l/3,2,0/,alo/2,4,2,2,4,0,0,33*0/
c      data al/0.,1.,1.,0.,1.,0.,0.,33*0./
c      data aj/0.5,1.5,0.5,0.5,1.5,0.,0.,33*0./
c      data en/-30.5,-15.9,-6.0,-35.2,-22.,0.,0.,33*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.7,1.73,1.7,0.,0.,33*0./
cccccccccccc  O15L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/6/,l/3,2,1/,alo/2,4,2,2,4,1,0,33*0/
c      data al/0.,1.,1.,0.,1.,0.,0.,33*0./
c      data aj/0.5,1.5,0.5,0.5,1.5,0.5,0.,33*0./
c      data en/-30.5,-15.9,-6.0,-35.2,-22.,-10.,0.,33*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.7,1.73,1.7,1.7,0.,33*0./

cccccccccccc  O15  ccccccccccccccccccccccccccccccccccccc
c      Data ln/6/,l/3,3,0/,alo/2,4,2,2,4,1,0,33*0/
c      data al/0.,1.,1.,0.,1.,1.,0.,33*0./
c      data aj/0.5,1.5,0.5,0.5,1.5,0.5,0.,33*0./
c      data en/-35.5,-25.9,-15.0,-35.2,-22.,-11.,0.,33*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.7,1.73,1.7,1.72,0.,33*0./
cccccccccccc  O16L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/7/,l/3,3,1/,alo/2,4,2,2,4,1,1,33*0/
c      data al/0.,1.,1.,0.,1.,1.,0.,33*0./
c      data aj/0.5,1.5,0.5,0.5,1.5,0.5,0.5,33*0./
c      data en/-35.5,-25.9,-15.0,-35.2,-22.,-11.,-10.,33*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.7,1.73,1.7,1.72,1.7,33*0./

cccccccccccc  O16  ccccccccccccccccccccccccccccccccccccc
c      Data ln/6/,l/3,3,0/,alo/2,4,2,2,4,2,0,33*0/
c      data al/0.,1.,1.,0.,1.,1.,0.,33*0./
c      data aj/0.5,1.5,0.5,0.5,1.5,0.5,0.,33*0./
c      data en/-32.5,-17.9,-11.0,-36.2,-20.,-14.,0.,33*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.7,1.73,1.7,1.72,0.,33*0./
cccccccccccc  O17L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/7/,l/3,3,1/,alo/2,4,2,2,4,2,1,12*0/
c      data al/0.,1.,1.,0.,1.,1.,0.,12*0./
c      data aj/0.5,1.5,0.5,0.5,1.5,0.5,0.5,12*0./
c      data en/-35.5,-25.9,-15.0,-35.2,-22.,-11.,-10.,12*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.7,1.73,1.7,1.72,1.7,12*0./

cccccccccccc  O18  ccccccccccccccccccccccccccccccccccccc
c      Data ln/7/,l/3,4,0/,alo/2,4,2,2,4,2,2,12*0/
c      data al/0.,1.,1.,0.,1.,1.,2.,12*0./
c      data aj/0.5,1.5,0.5,0.5,1.5,0.5,2.5,12*0./
c      data en/-35.5,-25.9,-15.0,-35.2,-22.,-11.,-4.,12*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.7,1.73,1.7,1.72,1.73,12*0./
cccccccccccc  O19L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/8/,l/3,4,1/,alo/2,4,2,2,4,2,2,1,11*0/
c      data al/0.,1.,1.,0.,1.,1.,2.,0.,11*0./
c      data aj/0.5,1.5,0.5,0.5,1.5,0.5,2.5,0.5,11*0./
c      data en/-35.5,-25.9,-15.0,-35.2,-22.,-11.,-4.,-10.,11*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.7,1.73,1.7,1.72,1.73,1.7,11*0./

cccccccccccc  O20  ccccccccccccccccccccccccccccccccccccc
c      Data ln/7/,l/3,4,0/,alo/2,4,2,2,4,2,4,12*0/
c      data al/0.,1.,1.,0.,1.,1.,2.,12*0./
c      data aj/0.5,1.5,0.5,0.5,1.5,0.5,2.5,12*0./
c      data en/-45.5,-29.9,-15.0,-30.2,-22.,-11.,-4.,12*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.7,1.73,1.7,1.72,1.73,12*0./
cccccccccccc  O21L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/8/,l/3,4,1/,alo/2,4,2,2,4,2,4,1,11*0/
c      data al/0.,1.,1.,0.,1.,1.,2.,0.,11*0./
c      data aj/0.5,1.5,0.5,0.5,1.5,0.5,2.5,0.5,11*0./
c      data en/-45.5,-29.9,-15.0,-30.2,-22.,-11.,-4.,-10.,11*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.7,1.73,1.7,1.72,1.73,1.7,11*0./

cccccccccccc  O22  ccccccccccccccccccccccccccccccccccccc
c      Data ln/7/,l/3,4,0/,alo/2,4,2,2,4,2,6,12*0/
c      data al/0.,1.,1.,0.,1.,1.,2.,12*0./
c      data aj/0.5,1.5,0.5,0.5,1.5,0.5,2.5,12*0./
c      data en/-45.5,-29.9,-15.0,-30.2,-22.,-11.,-4.,12*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.7,1.73,1.7,1.72,1.73,12*0./
cccccccccccc  O23L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/8/,l/3,4,1/,alo/2,4,2,2,4,2,6,1,11*0/
c      data al/0.,1.,1.,0.,1.,1.,2.,0.,11*0./
c      data aj/0.5,1.5,0.5,0.5,1.5,0.5,2.5,0.5,11*0./
c      data en/-45.5,-29.9,-15.0,-30.2,-22.,-11.,-4.,-10.,11*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.7,1.73,1.7,1.72,1.73,1.7,11*0./

ccccccccccc F14  ccccc cccccccccccccccccccccccccccccc
c      Data ln/6/,l/4,2,0/,alo/2,4,2,1,2,3,0,6*0,6*0/
c      data al/0.,1.,1.,2.,0.,1.,0.,6*0.,6*0./
c      data aj/.5,1.5,.5,2.5,0.5,1.5,0.,6*0.,6*0./
c      data en/-30.,-20.,-10,-1.,-35.4,-20.,0.,6*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,1.7,13*0./
ccccccccccc F15L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/7/,l/4,2,1/,alo/2,4,2,1,2,3,1,6*0,6*0/
c      data al/0.,1.,1.,2.,0.,1.,0.,6*0.,6*0./
c      data aj/.5,1.5,.5,2.5,0.5,1.5,0.5,6*0.,6*0./
c      data en/-30.,-20.,-10.,-1.,-30.,-20.,-12.,6*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,1.7,1.7,12*0./

cccccccccccc  F15  ccccccccccccccccccccccccccccccccccccc
c      Data ln/6/,l/4,2,0/,alo/2,4,2,1,2,4,0,0,11*0/
c      data al/0.,1.,1.,2.,0.,1.,0.,0.,11*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.,0.,11*0./
c      data en/-32.5,-16.9,-10.0,-2.0,-40.2,-24.,0.,0.,11*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.7,0.,0.,11*0./
cccccccccccc  F16L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/7/,l/4,2,1/,alo/2,4,2,1,2,4,1,0,11*0/
c      data al/0.,1.,1.,2.,0.,1.,0.,0.,11*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,0.,11*0./
c      data en/-31.,-15.,-7.,-1.,-39.,-23.,-14.,0.,11*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.7,0.,11*0./

ccccccccccc F16  ccccc cccccccccccccccccccccccccccccc
c      Data ln/7/,l/4,3,0/,alo/2,4,2,1,2,4,1,6*0,6*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,6*0.,6*0./
c      data aj/.5,1.5,.5,2.5,0.5,1.5,0.5,6*0.,6*0./
c      data en/-30.,-15.,-8.,-1.,-38.,-23.,-16.,6*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,1.7,1.7,12*0./
ccccccccccc F17L  ccccc cccccccccccccccccccccccccccccc
c      Data ln/8/,l/4,3,1/,alo/2,4,2,1,2,4,1,1,5*0,27*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,0.,5*0.,27*0./
c      data aj/.5,1.5,.5,2.5,0.5,1.5,0.5,0.5,5*0.,27*0./
c      data en/-31.,-16.,-9.,-2.,-39.,-23.,-17.,-13.,5*0.,27*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.73,1.7,1.7,1.7,1.7,1.7,32*0./

cccccccccccc  F23  ccccccccccccccccccccccccccccccccccccc
c      Data ln/8/,l/4,4,0/,alo/2,4,2,1,2,4,2,6,11*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,11*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,11*0./
c      data en/-45.5,-29.9,-22.0,-15.0,-30.2,-22.,-11.,-4.,11*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.73,11*0./

cccccccccccc  Ne16  ccccccccccccccccccccccccccccccccccccc
c      Data ln/6/,l/4,2,0/,alo/2,4,2,2,2,4,0,0,32*0/
c      data al/0.,1.,1.,2.,0.,1.,0.,0.,32*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.,0.,32*0./
c      data en/-32.5,-16.9,-10.0,-2.0,-40.2,-24.,0.,0.,32*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.7,0.,0.,32*0./
cccccccccccc  Ne17L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/7/,l/4,2,1/,alo/2,4,2,2,2,4,1,0,32*0/
c      data al/0.,1.,1.,2.,0.,1.,0.,0.,32*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,0.,32*0./
c      data en/-28.,-13.,-6.,-0.2,-40.2,-25.,-15.,0.,32*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.7,0.,32*0./

cccccccccccc  Ne17  ccccccccccccccccccccccccccccccccccccc
c      Data ln/7/,l/4,3,0/,alo/2,4,2,2,2,4,1,0,32*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,0.,32*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,0.,32*0./
c      data en/-32.5,-16.9,-10.0,-2.0,-40.2,-24.,-18.,0.,32*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,0.,32*0./
cccccccccccc  Ne18L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/8/,l/4,3,1/,alo/2,4,2,2,2,4,1,1,32*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,0.,32*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,0.5,32*0./
c      data en/-32.5,-16.9,-10.0,-2.0,-40.2,-24.,-18.,-10.,32*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.7,32*0./

cccccccccccc  Ne18  ccccccccccccccccccccccccccccccccccccc
c      Data ln/7/,l/4,3,0/,alo/2,4,2,2,2,4,2,0,32*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,0.,32*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,0.,32*0./
c      data en/-32.5,-16.9,-10.0,-2.0,-40.2,-24.,-18.,0.,32*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,0.,32*0./
cccccccccccc  Ne19L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/8/,l/4,3,1/,alo/2,4,2,2,2,4,2,1,32*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,0.,32*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,0.5,32*0./
c      data en/-32.5,-16.9,-10.0,-2.0,-40.2,-24.,-18.,-10.,32*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.7,32*0./

cccccccccccc  Ne20  ccccccccccccccccccccccccccccccccccccc
c      Data ln/8/,l/4,4,0/,alo/2,4,2,2,2,4,2,2,11*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,11*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,11*0./
c      data en/-35.5,-20.9,-14.0,-6.0,-40.2,-22.,-18.,-10.,11*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.73,11*0./
cccccccccccc  Ne21L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/9/,l/4,4,1/,alo/2,4,2,2,2,4,2,2,1,10*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,10*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,0.5,10*0./
c      data en/-35.5,-20.9,-14.0,-6.0,-40.2,-22.,-18.,-10.,-10.,10*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.73,1.7,10*0./

cccccccccccc  Ne22  ccccccccccccccccccccccccccccccccccccc
c      Data ln/8/,l/4,4,0/,alo/2,4,2,2,2,4,2,4,11*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,11*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,11*0./
c      data en/-45.5,-29.9,-22.0,-15.0,-30.2,-22.,-11.,-4.,11*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.73,11*0./
cccccccccccc  Ne23L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/9/,l/4,4,1/,alo/2,4,2,2,2,4,2,4,1,10*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,10*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,0.5,10*0./
c      data en/-45.5,-29.9,-22.0,-15.0,-30.2,-22.,-11.,-4.,-10.,10*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.73,1.7,10*0./

cccccccccccc  Ne24  ccccccccccccccccccccccccccccccccccccc
c      Data ln/8/,l/4,4,0/,alo/2,4,2,2,2,4,2,6,11*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,11*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,11*0./
c      data en/-45.5,-29.9,-22.0,-15.0,-30.2,-22.,-11.,-4.,11*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.73,11*0./

cccccccccccc  Na19  ccccccccccccccccccccccccccccccccccccc
c      Data ln/7/,l/4,3,0/,alo/2,4,2,3,2,4,2,0,32*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,0.,32*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,0.,32*0./
c      data en/-34.,-18.,-12.,-4.,-41.,-26.,-20.,0.,32*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,0.,32*0./
cccccccccccc  Na20L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/8/,l/4,3,1/,alo/2,4,2,3,2,4,2,1,32*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,0.,32*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,0.5,32*0./
c      data en/-34.,-18.,-12.,-4.,-41.,-26.,-20.,-10.,32*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.7,32*0./

cccccccccccc  Na20  ccccccccccccccccccccccccccccccccccccc
c      Data ln/8/,l/4,4,0/,alo/2,4,2,3,2,4,2,1,32*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,32*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,32*0./
c      data en/-34.,-18.,-12.,-4.,-41.,-26.,-20.,-12.,32*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.73,32*0./

cccccccccccc  Na25  ccccccccccccccccccccccccccccccccccccc
c      Data ln/8/,l/4,4,0/,alo/2,4,2,3,2,4,2,6,11*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,11*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,11*0./
c      data en/-45.5,-29.9,-22.0,-15.0,-30.2,-22.,-11.,-4.,11*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.73,11*0./

cccccccccccc  Mg19  ccccccccccccccccccccccccccccccccccccc
c      Data ln/7/,l/4,3,0/,alo/2,4,2,4,2,4,1,0,32*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,0.,32*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,0.,32*0./
c      data en/-32.,-15.,-9.,-0.2,-44.,-28.,-23.,0.,32*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,0.,32*0./
cccccccccccc  Mg20L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/8/,l/4,3,1/,alo/2,4,2,4,2,4,1,1,32*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,0.,32*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,0.5,32*0./
c      data en/-32.,-15.,-9.,-1.,-44.,-28.,-23.,-16.,32*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.7,32*0./

cccccccccccc  Mg20  ccccccccccccccccccccccccccccccccccccc
c      Data ln/7/,l/4,3,0/,alo/2,4,2,4,2,4,2,0,11*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,0.,11*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,0.,11*0./
c      data en/-35.5,-19.9,-13.0,-5.0,-43.2,-27.,-21.,0.,11*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,0.,11*0./
cccccccccccc  Mg21L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/8/,l/4,3,1/,alo/2,4,2,4,2,4,2,1,11*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,0.,11*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,0.5,11*0./
c      data en/-35.5,-19.9,-13.0,-5.0,-43.2,-27.,-21.,-10.,11*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.7,11*0./

cccccccccccc  Mg22  ccccccccccccccccccccccccccccccccccccc
c      Data ln/8/,l/4,4,0/,alo/2,4,2,4,2,4,2,2,11*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,11*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,11*0./
c      data en/-35.5,-19.9,-13.0,-5.0,-43.2,-27.,-21.,-13.,11*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.73,11*0./
cccccccccccc  Mg23L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/9/,l/4,4,1/,alo/2,4,2,4,2,4,2,2,1,10*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,10*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,0.5,10*0./
c      data en/-35.5,-19.9,-13.0,-5.0,-43.2,-27.,-21.,-13.,-10.,10*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.73,1.7,10*0./

cccccccccccc  Mg26  ccccccccccccccccccccccccccccccccccccc
c      Data ln/8/,l/4,4,0/,alo/2,4,2,4,2,4,2,6,11*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,11*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,11*0./
c      data en/-45.5,-29.9,-22.0,-15.0,-30.2,-22.,-11.,-4.,11*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.73,11*0./

cccccccccccc  Al27  ccccccccccccccccccccccccccccccccccccc
c      Data ln/8/,l/4,4,0/,alo/2,4,2,5,2,4,2,6,11*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,11*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,11*0./
c      data en/-45.5,-29.9,-22.0,-15.0,-30.2,-22.,-11.,-4.,11*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.73,11*0./

ccccccc/////// Si izotopes /////////////////ccccccccccccccccccccccccccc
cccccccccccc  Si21  cccccccc doesnt work !!! cccccccccccccc
c      Data ln/7/,l/4,3,0/,alo/2,4,2,6,2,4,1,0,11*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,0,11*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,0,11*0./
c      data en/-22.,-11.,-7.0,-1.0,-39.2,-28.,-24.,0,11*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.73,11*0./

cccccccccccc  Si22  ccccccccccccccccccccccccccccccccccccc
c      Data ln/7/,l/4,3,0/,alo/2,4,2,6,2,4,2,0,32*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,0,32*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,0,32*0./
c      data en/-24.,-13.,-9.0,-2.0,-39.2,-28.,-24.,0,32*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c	data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.73,32*0./
cccccccccccc  Si23L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/8/,l/4,3,1/,alo/2,4,2,6,2,4,2,1,32*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,0.,32*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,0.5,32*0./
c      data en/-24.,-13.,-9.0,-2.0,-39.2,-28.,-24.,-10.,32*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.73,32*0./

ccccccc  Si22 for more 1 p and 3 n upper subshells (empty) c1 for Skp
ccccccc c2 doesnt work for Sly230b cccccccccccccccccccccccccccccccccc
c      Data ln/12/,l/5,7,0/,alo/2,4,2,6,0,2,4,2,0,0,9*0/
c      data al/0.,1.,1.,2.,0.,0.,1.,1.,2,0.,2.,3.,7*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,7*0./
c1      data en/-24.,-13.,-9.0,-2.0,-0.1,
c1     *-39.2,-28.,-24.,-17.,-14,-11.,-5.,7*0./
c2        data en/-32.,-16.,-11.,-2.0,-0.1,
c2     *-45.2,-30.,-25.,-17.,-14,-11.,-5.,7*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.74,1.76,1.8,
c     *1.73,1.7,1.72,1.73,1.73,1.74,1.74,7*0./

cccccccccccc  Si23 for more 1 p and 3 n upper subshells (empty)ccc
c      Data ln/12/,l/5,7,0/,alo/2,4,2,6,0,2,4,2,1,0,9*0/
c      data al/0.,1.,1.,2.,0.,0.,1.,1.,2,0.,2.,3.,7*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,7*0./
c      data en/-24.,-13.,-9.0,-2.0,-0.1,
c     *-39.2,-28.,-24.,-17.,-14,-11.,-5.,7*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.71,1.73,1.73,
c     *1.73,1.7,1.72,1.73,1.73,1.74,1.74,7*0./

cccccccccccc  Si23  ccccccccccccccccccccccccccccccccccccc
c      Data ln/8/,l/4,4,0/,alo/2,4,2,6,2,4,2,1,32*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,32*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,32*0./
c      data en/-29.,-17.,-13.0,-7.0,-37.2,-26.,-22.,-15.,32*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.73,32*0./
cccccccccccc  Si24L  ccccccccccccccccccccccccccccccccccccc
c      Data ln/9/,l/4,4,1/,alo/2,4,2,4,2,4,2,1,1,31*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,31*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,0.5,31*0./
c      data en/-29.,-17.,-13.0,-7.0,-37.2,-26.,-22.,-15.,-10.,31*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.73,1.73,31*0./

cccccccccccc  Si24 for more 1 p and 3 n upper subshells (empty)ccc
c      Data ln/12/,l/5,7,0/,alo/2,4,2,6,0,2,4,2,2,0,9*0/
c      data al/0.,1.,1.,2.,0.,0.,1.,1.,2,0.,2.,3.,7*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,7*0./
c      data en/-24.,-13.,-9.0,-4.,-3.,
c     *-39.2,-28.,-24.,-17.,-14,-11.,-5.,7*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.71,1.73,1.73,
c     *1.73,1.7,1.72,1.73,1.73,1.74,1.74,7*0./

cccccccccccc  Si24  ccccccccccccccccccccccccccccccccccccc
c      Data ln/9/,l/5,4,0/,alo/2,4,2,6,0,2,4,2,2,10*0/
c      data al/0.,1.,1.,2.,0.,0.,1.,1.,2.,10*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,0.5,1.5,0.5,2.5,10*0./
c      data en/-29.,-17.,-13.0,-7.0,-6.0,-37.2,-26.,-22.,-15.,10*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.71,1.73,1.73,1.73,1.7,1.72,1.73,10*0./

cccccccccccc  Si25 for more 1 p and 3 n upper subshells (empty)ccc
c      Data ln/12/,l/5,7,0/,alo/2,4,2,6,0,2,4,2,3,0,9*0/
c      data al/0.,1.,1.,2.,0.,0.,1.,1.,2,0.,2.,3.,7*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,7*0./
c      data en/-24.,-13.,-9.,-4.,-3.,
c     *-38.2,-27.,-23.,-16.,-14.,-10.,-5.,7*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.71,1.73,1.73,
c     *1.73,1.7,1.72,1.73,1.73,1.74,1.74,7*0./

cccccccccccc  Si25  ccccccccccccccccccccccccccccccccccccc
c      Data ln/8/,l/4,4,0/,alo/2,4,2,6,2,4,2,3,11*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,11*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,11*0./
c      data en/-29.,-17.,-13.0,-7.0,-37.2,-26.,-22.,-15.,11*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.73,11*0./

cccccccccccc  Si26 for more 1 p and 3 n upper subshells (empty)ccc
c      Data ln/12/,l/5,7,0/,alo/2,4,2,6,0,2,4,2,4,0,9*0/
c      data al/0.,1.,1.,2.,0.,0.,1.,1.,2,0.,2.,3.,7*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,7*0./
c      data en/-24.,-13.,-9.,-4.,-3.,
c     *-38.2,-27.,-23.,-16.,-14.,-10.,-5.,7*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.71,1.73,1.73,
c     *1.73,1.7,1.72,1.73,1.73,1.74,1.74,7*0./

cccccc not workingcccccc  Si26 for more 2 p and 4 n upper subshells (empty)ccc
c      Data ln/13/,l/6,7,0/,alo/2,4,2,6,0,0,2,4,2,4,0,8*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2,0.,2.,3.,6*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,6*0./
c      data en/-29.,-17.,-13.,-7.,-6.,-3.,
c     *-38.2,-27.,-23.,-16.,-14.,-10.,-5.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.71,1.73,1.73,1.75,
c     *1.73,1.7,1.72,1.73,1.73,1.74,1.74,6*0./

cccccccccccc  Si26  ccccccccccccccccccccccccccccccccccccc
c      Data ln/8/,l/4,4,0/,alo/2,4,2,6,2,4,2,4,11*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,11*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,11*0./
c      data en/-45.5,-29.9,-22.0,-15.0,-30.2,-22.,-11.,-4.,11*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.73,11*0./

cccccccccccc  Si27 for more 1 p and 3 n upper subshells (empty)ccc
c      Data ln/12/,l/5,7,0/,alo/2,4,2,6,0,2,4,2,5,0,9*0/
c      data al/0.,1.,1.,2.,0.,0.,1.,1.,2,0.,2.,3.,7*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,7*0./
c      data en/-24.,-13.,-9.,-4.,-3.,
c     *-38.2,-27.,-23.,-16.,-14.,-10.,-5.,7*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.71,1.73,1.73,
c     *1.73,1.7,1.72,1.73,1.73,1.74,1.74,7*0./

cccccccccccc  Si27  ccccccccccccccccccccccccccccccccccccc
c      Data ln/8/,l/4,4,0/,alo/2,4,2,6,2,4,2,5,11*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,11*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,11*0./
c      data en/-45.5,-29.9,-22.0,-15.0,-30.2,-22.,-11.,-4.,11*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.73,11*0./

cccccccccccc  Si28 for more 2 p and 3 n upper subshells ccc
c      Data ln/13/,l/6,7,0/,alo/2,4,2,6,0,0,2,4,2,6,30*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2,0.,2.,3.,27*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,27*0./
c      data en/-24.,-13.,-9.,-4.,-3.,-1.,
c     *-38.2,-27.,-23.,-16.,-14.,-10.,-3.,27*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.73,
c     *1.73,1.7,1.72,1.73,1.73,1.74,1.74,27*0./
cccccccccccc  Si28 for more 2 p and 3 n upper subshells (SAMi)ccc
c      Data ln/13/,l/6,7,0/,alo/2,4,2,6,0,0,2,4,2,6,30*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2,0.,2.,3.,27*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,27*0./
c      data en/-43.,-25.,-23.,-9.,-7.,-5.,
c     *-50.,-31.,-28.,-15.,-12.,-10.,-1.,27*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.73,
c     *1.73,1.7,1.72,1.73,1.73,1.74,1.74,27*0./

cccccccccccc  Si28 for more 2 p and 2 n upper subshells (empty) BCS ccc
c      Data ln/12/,l/6,6,0/,alo/1.99815,3.98547,1.98690,5.29037,0.60553
c     *,0.13358,1.99838,3.98727,1.98874,5.34609,0.56159,0.11794,28*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2,0.,2.,28*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,1.5,0.5,2.5,0.5,1.5,28*0./
c      data en/-41.1314,-24.6786,-20.3739,-10.4095,-7.1544,-2.7794,
c     *-47.3956,-30.4708,-26.1808,-15.8072,-12.4279,-7.9830,28*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.73,
c     *1.73,1.7,1.72,1.73,1.73,1.74,28*0./

cccccccccccc  Si28  ccccccccccccccccccccccccccccccccccccc
c      Data ln/13/,l/6,7,0/,alo/2,4,2,6,0,0,2,4,2,6,0,0,0,27*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2,0.,2.,3.,27*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,27*0./
c      data en/-43.,-25.,-23.,-9.,-7.,-5.,-50.,-31.,-28.,-15.,-12.,-10.,
c     *-1.,27*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.73,1.73,1.7,1.72,1.73,1.73,
c     *1.74,1.74,27*0./

cccccccccccc  Si29 for more 2 p and 2 n upper subshells (empty)ccc
c      Data ln/12/,l/6,6,0/,alo/2,4,2,6,0,0,2,4,2,6,1,29*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2,0.,2.,28*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,1.5,0.5,2.5,0.5,1.5,28*0./
c      data en/-31.,-20.,-16.,-9.,-8.,-4.,
c     *-37.2,-25.,-21.,-15.,-13.,-8.,28*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.73,
c     *1.73,1.7,1.72,1.73,1.73,1.74,28*0./

cccccccccccc  Si29  ccccccccccccccccccccccccccccccccccccc
c      Data ln/9/,l/4,5,0/,alo/2,4,2,6,2,4,2,6,1,10*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,10*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,0.5,10*0./
c      data en/-45.5,-29.9,-22.0,-15.0,-30.2,-22.,-11.,-4.,-1.,10*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.73,1.74,10*0./

cccccccccccc  Si30 for more 3 p and 3 n upper subshells (empty)ccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,0,0,0,2,4,2,6,2,28*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2,0.,2.,3.,26*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,26*0./
c      data en/-31.,-20.,-16.,-9.,-8.,-4.,-2.,
c     *-37.2,-25.,-21.,-15.,-13.,-8.,-3.,26*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.73,1.73,
c     *1.73,1.7,1.72,1.73,1.73,1.74,1.74,26*0./

cccccccccccc  Si30  ccccccccccccccccccccccccccccccccccccc
c      Data ln/9/,l/4,5,0/,alo/2,4,2,6,2,4,2,6,2,10*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,10*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,0.5,10*0./
c      data en/-45.5,-29.9,-22.0,-15.0,-30.2,-22.,-11.,-4.,-1.,10*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.73,1.74,10*0./

cccccccccccc  Si31 for more 3 p and 1 n upper subshells (empty)ccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,0,0,0,2,4,2,6,2,1,27*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2,0.,2.,3.,26*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,26*0./
c      data en/-33.,-22.,-19.,-12.,-10.,-6.,-3.,
c     *-34.,-25.,-21.,-15.,-13.,-8.,-4.,26*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.73,1.74,
c     *1.73,1.7,1.72,1.73,1.73,1.74,1.74,26*0./

cccccccccccc  Si31  ccccccccccccccccccccccccccccccccccccc
c      Data ln/10/,l/4,6,0/,alo/2,4,2,6,2,4,2,6,2,1,9*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,2.,9*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,0.5,1.5,9*0./
c      data en/-33.,-22.,-18.,-12.,-34.,-25.,-20.,-14.,-11.,-5,9*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.73,1.74,1.8,9*0./

cccccccccccc  Si32 for more 3 p and 1 n upper subshells (empty)ccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,0,0,0,2,4,2,6,2,2,27*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2,0.,2.,3.,26*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,26*0./
c      data en/-33.,-22.,-19.,-12.,-10.,-6.,-3.,
c     *-34.,-25.,-21.,-15.,-13.,-8.,-4.,26*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.73,1.74,
c     *1.73,1.7,1.72,1.73,1.73,1.74,1.74,26*0./

cccccccccccc  Si32  ccccccccccccccccccccccccccccccccccccc
c      Data ln/10/,l/4,6,0/,alo/2,4,2,6,2,4,2,6,2,2,9*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,2.,9*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,0.5,1.5,9*0./
c      data en/-33.,-22.,-18.,-12.,-34.,-25.,-20.,-14.,-11.,-5,9*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.73,1.74,1.8,9*0./

cccccccccccc  Si33 for more 3 p and 1 n upper subshells (empty)ccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,0,0,0,2,4,2,6,2,3,27*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2,0.,2.,3.,26*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,26*0./
c      data en/-33.,-22.,-19.,-12.,-10.,-6.,-3.,
c     *-34.,-25.,-21.,-15.,-13.,-8.,-4.,26*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.73,1.74,
c     *1.73,1.7,1.72,1.73,1.73,1.74,1.74,26*0./

cccccccccccc  Si33  ccccccccccccccccccccccccccccccccccccc
c      Data ln/10/,l/4,6,0/,alo/2,4,2,6,2,4,2,6,2,3,9*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,2.,9*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,0.5,1.5,9*0./
c      data en/-33.,-22.,-18.,-12.,-34.,-25.,-20.,-14.,-11.,-5,9*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.73,1.74,1.8,9*0./

cccccccccccc Si34 for more 3 p and 1 n upper subshells (empty)ccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,0,0,0,2,4,2,6,2,4,27*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2,0.,2.,3.,26*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,26*0./
c      data en/-33.,-22.,-19.,-12.,-10.,-6.,-3.,
c     *-34.,-25.,-21.,-15.,-13.,-8.,-4.,26*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.73,1.74,
c     *1.73,1.7,1.72,1.73,1.73,1.74,1.74,26*0./

cccccccccccc  Si34  ccccccccccccccccccccccccccccccccccccc
c      Data ln/10/,l/4,6,0/,alo/2,4,2,6,2,4,2,6,2,4,30*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,2.,30*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,0.5,1.5,30*0./
c      data en/-33.,-22.,-19.,-12.,-34.,-25.,-21.,-15.,-13.,-8.,30*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.7,1.72,1.73,1.73,1.74,30*0./

cccccccccccc  Si35 for more 3 p upper subshells (empty)ccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,0,0,0,2,4,2,6,2,4,1,26*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,
c     *0.,1.,1.,2,0.,2.,3.,26*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,26*0./
c      data en/-37.,-27.,-24.,-17.,-14.,-11.,-6.,
c     *-32.,-23.,-19.,-14.,-11.,-8.,-4.,26*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.73,1.74,
c     *1.73,1.7,1.72,1.73,1.73,1.74,1.74,26*0./

cccccccccccc  Si35  ccccccccccccccccccccccccccccccccccccc
c      Data ln/11/,l/4,7,0/,alo/2,4,2,6,2,4,2,6,2,4,1,29*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,2.,3.,29*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,29*0./
c      data en/-33.,-22.,-18.,-12.,-34.,-25.,-20.,-14.,-11.,-5.,-3.,
c     *29*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.7,1.71,1.73,1.73,1.7,1.72,1.73,1.74,1.8,1.8,29*0./

cccccccccccc  Si36 for more 3p upper subshells (empty)ccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,0,0,0,2,4,2,6,2,4,2,26*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,
c     *0.,1.,1.,2,0.,2.,3.,26*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,26*0./
c      data en/-41.,-31.,-28.,-20.,-18.,-15.,-10.,
c     *-32.,-23.,-19.,-14.,-11.,-8.,-4.,26*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.74,1.76,
c     *1.73,1.7,1.72,1.73,1.76,1.82,1.84,26*0./
cccccccccccc  Si36 for more 3p1n upper subshells (empty)ccc
c      Data ln/15/,l/7,8,0/,alo/2,4,2,6,0,0,0,2,4,2,6,2,4,2,26*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,
c     *0.,1.,1.,2,0.,2.,3.,1.,25*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,25*0./
c      data en/-44.,-31.,-28.,-19.,-15.,-12.,-6.,
c     *-41.,-29.,-25.,-16.,-12.,-10.,-5.,-1.,25*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.74,1.76,
c     *1.73,1.7,1.72,1.73,1.76,1.82,1.84,1.86,25*0./

cccccccccccc  Si36  ccccccccccccccccccccccccccccccccccccc
c      Data ln/11/,l/4,7,0/,alo/2,4,2,6,2,4,2,6,2,4,2,29*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,2.,3.,29*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,29*0./
c      data en/-33.,-22.,-18.,-12.,-34.,-25.,-20.,-14.,-11.,-5.,-3.,
c     *29*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.7,1.71,1.73,1.73,1.7,1.72,1.73,1.74,1.8,1.8,29*0./

cccccccccccc  Si37 for more 3 p upper subshells (empty)ccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,0,0,0,2,4,2,6,2,4,3,26*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,
c     *0.,1.,1.,2,0.,2.,3.,26*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,26*0./
c      data en/-41.,-31.,-28.,-20.,-18.,-15.,-10.,
c     *-32.,-23.,-19.,-14.,-11.,-8.,-4.,26*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.74,1.76,
c     *1.73,1.7,1.72,1.73,1.76,1.82,1.84,26*0./

cccccccccccc  Si37  ccccccccccccccccccccccccccccccccccccc
c      Data ln/11/,l/4,7,0/,alo/2,4,2,6,2,4,2,6,2,4,3,29*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,2.,3.,29*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,29*0./
c      data en/-33.,-22.,-18.,-12.,-34.,-25.,-20.,-14.,-11.,-5.,-3.,
c     *29*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.7,1.71,1.73,1.73,1.7,1.72,1.73,1.74,1.8,1.8,29*0./

cccccccccccc  Si38 for more 4 p upper subshells (empty) ccc
c      Data ln/15/,l/8,7,0/,alo/2,4,2,6,0,0,0,0,2,4,2,6,2,4,4,25*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,
c     *0.,1.,1.,2,0.,2.,3.,25*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,25*0./
c      data en/-48.,-34.,-30.,-21.,-16.,-14.,-8.,-4.,
c     *-46.,-30.,-26.,-16.,-13.,-9.,-4.,25*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.74,1.76,1.8,
c     *1.73,1.7,1.72,1.73,1.76,1.82,1.84,25*0./
cccccccccccc  Si38 for more 4p1n upper subshells (empty) ccc
c      Data ln/16/,l/8,8,0/,alo/2,4,2,6,0,0,0,0,2,4,2,6,2,4,4,25*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,
c     *0.,1.,1.,2,0.,2.,3.,1.,24*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,24*0./
c      data en/-48.,-34.,-30.,-21.,-16.,-14.,-8.,-4.,
c     *-46.,-30.,-26.,-16.,-13.,-9.,-4.,-1.,24*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.74,1.76,1.8,
c     *1.73,1.7,1.72,1.73,1.76,1.82,1.84,1.84,24*0./

cccccccccccc  Si38  ccccccccccccccccccccccccccccccccccccc
c      Data ln/11/,l/4,7,0/,alo/2,4,2,6,2,4,2,6,2,4,4,29*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,2.,3.,29*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,29*0./
c      data en/-33.,-22.,-18.,-12.,-34.,-25.,-20.,-14.,-11.,-5.,-3.,
c     *29*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.7,1.71,1.73,1.73,1.7,1.72,1.73,1.74,1.8,1.8,29*0./

cccccccccccc  Si39 for more 4 p upper subshells (empty)ccc
c      Data ln/15/,l/8,7,0/,alo/2,4,2,6,0,0,0,0,2,4,2,6,2,4,5,25*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,
c     *0.,1.,1.,2,0.,2.,3.,25*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,25*0./
c        data en/-48.,-35.,-31.,-22.,-17.,-15.,-9.,-5.,
c     *-46.,-31.,-27.,-17.,-13.,-10.,-4.,25*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.74,1.76,1.8,
c     *1.73,1.7,1.72,1.73,1.76,1.82,1.84,25*0./

cccccccccccc  Si39  ccccccccccccccccccccccccccccccccccccc
c      Data ln/11/,l/4,7,0/,alo/2,4,2,6,2,4,2,6,2,4,5,29*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,2.,3.,29*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,29*0./
c      data en/-33.,-22.,-18.,-12.,-34.,-25.,-20.,-14.,-11.,-5.,-3.,
c     *29*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.7,1.71,1.73,1.73,1.7,1.72,1.73,1.74,1.8,1.8,29*0./

cccccccccccc  Si40 for more 4 p upper subshells (empty)ccc
c      Data ln/15/,l/8,7,0/,alo/2,4,2,6,0,0,0,0,2,4,2,6,2,4,6,25*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,
c     *0.,1.,1.,2,0.,2.,3.,25*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,25*0./
c      data en/-48.,-35.,-31.,-22.,-17.,-15.,-9.,-5.,
c     *-46.,-31.,-27.,-17.,-13.,-10.,-4.,25*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.74,1.76,1.8,
c     *1.73,1.7,1.72,1.73,1.76,1.82,1.84,25*0./
cccccccccccc  Si40 for more 4p1n upper subshells (empty)ccc
c      Data ln/16/,l/8,8,0/,alo/2,4,2,6,0,0,0,0,2,4,2,6,2,4,6,25*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,
c     *0.,1.,1.,2,0.,2.,3.,1.,24*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,24*0./
c      data en/-50.,-36.,-33.,-23.,-19.,-17.,-11.,-6.,
c     *-46.,-31.,-28.,-17.,-13.,-10.,-4.,-1.,24*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.74,1.76,1.8,
c     *1.73,1.7,1.72,1.73,1.76,1.82,1.84,1.84,24*0./


cccccccccccc  Si40  ccccccccccccccccccccccccccccccccccccc
c      Data ln/11/,l/4,7,0/,alo/2,4,2,6,2,4,2,6,2,4,6,29*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,2.,3.,29*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,29*0./
c      data en/-33.,-22.,-18.,-12.,-34.,-25.,-20.,-14.,-11.,-5.,-3.,
c     *29*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.7,1.71,1.73,1.73,1.7,1.72,1.73,1.74,1.8,1.8,29*0./

cccccccccccc  Si41 for more 4 p upper subshells (empty)ccc
c      Data ln/15/,l/8,7,0/,alo/2,4,2,6,0,0,0,0,2,4,2,6,2,4,7,25*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,
c     *0.,1.,1.,2,0.,2.,3.,25*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,25*0./
c      data en/-48.,-35.,-31.,-22.,-17.,-15.,-9.,-5.,
c     *-46.,-31.,-27.,-17.,-13.,-10.,-4.,25*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.74,1.76,1.8,
c     *1.73,1.7,1.72,1.73,1.76,1.82,1.84,25*0./

cccccccccccc  Si41  ccccccccccccccccccccccccccccccccccccc
c      Data ln/11/,l/4,7,0/,alo/2,4,2,6,2,4,2,6,2,4,7,29*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,2.,3.,29*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,29*0./
c      data en/-33.,-22.,-18.,-12.,-34.,-25.,-20.,-14.,-11.,-5.,-3.,
c     *29*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.7,1.71,1.73,1.73,1.7,1.72,1.73,1.74,1.8,1.8,29*0./


cccccccccccc  Si42 for more 4 p upper subshells (empty)ccc
c      Data ln/15/,l/8,7,0/,alo/2,4,2,6,0,0,0,0,2,4,2,6,2,4,8,25*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,
c     *0.,1.,1.,2,0.,2.,3.,25*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,25*0./
c        data en/-50.,-38.,-35.,-25.,-20.,-19.,-13.,-8.,
c     *-47.,-32.,-29.,-17.,-13.,-11.,-4.,25*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.74,1.76,1.8,
c     *1.73,1.7,1.72,1.73,1.76,1.82,1.84,25*0./
cccccccccccc  Si42 for more 4p1n upper subshells (empty)ccc
c      Data ln/16/,l/8,8,0/,alo/2,4,2,6,0,0,0,0,2,4,2,6,2,4,8,25*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,
c     *0.,1.,1.,2.,0.,2.,3.,1.,24*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,24*0./
c      data en/-51.,-38.,-35.,-25.,-21.,-19.,-13.,-8.,
c     *-47.,-32.,-29.,-17.,-14.,-11.,-4.,-0.5,24*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.71,1.71,1.73,1.73,1.74,1.76,1.8,
c     *1.73,1.7,1.72,1.73,1.76,1.82,1.84,1.86,24*0./

cccccccccccc  Si42  ccccccccccccccccccccccccccccccccccccc
c      Data ln/11/,l/4,7,0/,alo/2,4,2,6,2,4,2,6,2,4,8,29*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,2.,3.,29*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,29*0./
c      data en/-33.,-22.,-18.,-12.,-34.,-25.,-20.,-14.,-11.,-5.,-3.,
c     *29*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.67,1.7,1.71,1.73,1.73,1.7,1.72,1.73,1.74,1.8,1.8,29*0./

cccccccccccc  Si43 for more 5 p upper subshells (empty)ccc
cccccccccccc  c1 for Skp c2 for Sly230b cccccccccccccccccc
c      Data ln/17/,l/9,8,0/,alo/2,4,2,6,0,0,0,0,0,2,4,2,6,2,4,8,1,2*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,3.,
c     *0.,1.,1.,2,0.,2.,3.,1.,2*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,2.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,2*0./
c1      data en/-44.,-34.,-32.,-24.,-21.,-19.,-14.,-10.,-7.,
c1     *-32.,-23.,-19.,-14.,-11.,-8.,-4.,-1.,2*0./
c2      data en/-51.,-38.,-35.,-25.,-21.,-19.,-13.,-8.,-5.,
c2     *-46.,-31.,-28.,-17.,-13.,-10.,-4.,-1.,2*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c1	data bf/1.67,1.71,1.71,1.73,1.73,1.74,1.76,1.8,1.84,
c1     *1.73,1.7,1.72,1.74,1.76,1.84,1.86,1.9,2*0./
c2	data bf/1.62,1.7,1.71,1.73,1.73,1.74,1.76,1.8,1.84,
c2     *1.83,1.8,1.82,1.83,1.86,1.92,1.94,2.1,2*0./

cccccccccccc  Si43 (c1 for Skp)(c2 for Sly230b) cccccccccc
c      Data ln/12/,l/4,8,0/,alo/2,4,2,6,2,4,2,6,2,4,8,1,7*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,2.,3.,1.,7*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,7*0./
c1      data en/-44.,-34.,-31.,-24.,
c1     *-31.,-22.,-20.,-14.,-10.,-7.,-4.,-1.,7*0./
c2      data en/-51.,-38.,-35.,-25.,
c2     *-46.,-31.,-28.,-17.,-13.,-10.,-4.,-1.,7*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c2	data bf/1.62,1.7,1.71,1.73,
c2     *1.83,1.8,1.82,1.83,1.86,1.92,1.94,2.1,7*0./
c1      data bf/1.62,1.7,1.7,1.74,
c1     *1.67,1.67,1.7,1.73,1.76,1.92,1.98,2.4,7*0./

cccccccccccc  Si44 for more 5 p upper subshells (empty)ccc
cccccccccccc  c1 for Skp c2 for Sly230b cccccccccccccccccc
c      Data ln/17/,l/9,8,0/,alo/2,4,2,6,0,0,0,0,0,2,4,2,6,2,4,8,2,2*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,3.,
c     *0.,1.,1.,2,0.,2.,3.,1.,2*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,2.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,2*0./
c1      data en/-44.,-34.,-32.,-24.,-21.,-19.,-14.,-10.,-7.,
c1     *-32.,-23.,-19.,-14.,-11.,-8.,-4.,-1.,2*0./
c2      data en/-51.,-38.,-35.,-25.,-21.,-19.,-13.,-8.,-5.,
c2     *-46.,-31.,-28.,-17.,-13.,-10.,-4.,-1.,2*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c1	data bf/1.67,1.71,1.71,1.73,1.73,1.74,1.76,1.8,1.84,
c1     *1.73,1.7,1.72,1.74,1.76,1.84,1.86,1.9,2*0./
c	data bf/1.62,1.7,1.71,1.73,1.73,1.74,1.76,1.8,1.84,
c     *1.83,1.8,1.82,1.83,1.86,1.92,1.94,2.1,2*0./

cccccccccccc  Si44 (c2 for Sly230b)  ccccccccccccccccccccccccc
c      Data ln/12/,l/4,8,0/,alo/2,4,2,6,2,4,2,6,2,4,8,2,7*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,2.,3.,1.,7*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,7*0./
c1      data en/-44.,-34.,-31.,-24.,
c1     *-31.,-22.,-20.,-14.,-10.,-7.,-4.,-1.,7*0./
c2      data en/-51.,-38.,-35.,-25.,
c2     *-46.,-31.,-28.,-17.,-13.,-10.,-4.,-1.,7*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c1	data bf/1.67,1.7,1.71,1.73,
c1     *1.73,1.7,1.72,1.73,1.76,1.82,1.84,1.9,7*0./
c0      data bf/1.62,1.7,1.7,1.74,
c0     *1.67,1.67,1.7,1.73,1.76,1.92,1.98,2.4,7*0./
c2	data bf/1.62,1.7,1.71,1.73,
c2     *1.83,1.8,1.82,1.83,1.86,1.92,1.94,2.1,7*0./

cccccccccccc  Si45 for more 5 p upper subshells (empty)ccc
cccccccccccc  c1 for Skp c2 for Sly230b cccccccccccccccccc
c      Data ln/17/,l/9,8,0/,alo/2,4,2,6,0,0,0,0,0,2,4,2,6,2,4,8,3,2*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,3.,
c     *0.,1.,1.,2,0.,2.,3.,1.,2*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,2.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,2*0./
c1      data en/-44.,-34.,-32.,-24.,-21.,-19.,-14.,-10.,-7.,
c1     *-32.,-23.,-19.,-14.,-11.,-8.,-4.,-1.,2*0./
c2      data en/-51.,-38.,-35.,-25.,-21.,-19.,-13.,-8.,-5.,
c2     *-46.,-31.,-28.,-17.,-13.,-10.,-4.,-1.,2*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c1	data bf/1.67,1.71,1.71,1.73,1.73,1.74,1.76,1.8,1.84,
c1     *1.73,1.7,1.72,1.74,1.76,1.84,1.86,1.9,2*0./
c	data bf/1.62,1.7,1.71,1.73,1.73,1.74,1.76,1.8,1.84,
c     *1.83,1.8,1.82,1.83,1.86,1.92,1.94,2.1,2*0./

cccccccccccc  Si45  cccc(doesnt work for Skp) (c2 for Sly230b)ccccccc
c      Data ln/12/,l/4,8,0/,alo/2,4,2,6,2,4,2,6,2,4,8,3,7*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,2.,3.,1.,7*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,7*0./
c1      data en/-46.,-36.,-33.,-26.,
c1     *-31.,-22.,-20.,-13.,-11.,-7.,-4.,-3.,7*0./
c2      data en/-51.,-38.,-35.,-25.,
c2     *-46.,-31.,-28.,-17.,-13.,-10.,-4.,-1.,7*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c1	data bf/1.67,1.7,1.71,1.73,
c1     *1.73,1.7,1.72,1.73,1.76,1.82,1.84,1.9,7*0./
c2      data bf/1.62,1.7,1.7,1.74,
c2     *1.67,1.67,1.7,1.73,1.76,1.92,2.,2.4,7*0./

cccccccccccc  15P29 (c2 for SkIII)  ccccccccccccccccccccccccccccccccccc
c      Data ln/9/,l/5,4,0/,alo/2,4,2,6,1,2,4,2,6,10*0/
c      data al/0.,1.,1.,2.,0.,0.,1.,1.,2.,10*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,0.5,1.5,0.5,2.5,10*0./
c      data en/-38.5,-23.9,-19.0,-10.0,-7.0,-43.2,-29.,-25.,-15.,10*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c1      data bf/1.62,1.7,1.7,1.74,1.77,1.62,1.65,1.7,1.74,10*0./
c2      data bf/1.65,1.7,1.77,1.74,1.8,1.65,1.67,1.7,1.74,10*0./

cccccccccccc  S25 + 3n upper subshells (doesnt work)  ccccccccccccccc
c      Data ln/12/,l/5,7,0/,alo/2,4,2,6,2,2,4,2,1,0,0,0,7*0/
c      data al/0.,1.,1.,2.,0.,0.,1.,1.,2.,0.,2.,3.,7*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,7*0./
c1      data en/-45.5,-29.9,-22.0,-15.0,-10.0,-30.2,-22.,-11.,-4.,10*0./
c      data en/-24.2,-15.,-10.5,-5.,-2.,
c     *-40.,-29.,-25.,-18.,-16.,-12.,-7.,7*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.88,1.86,1.84,1.78,1.76,
c     *1.65,1.67,1.7,1.7,1.74,1.74,1.76,7*0./
c2      data bf/1.67,1.77,1.8,1.8,1.9,1.67,1.67,1.7,1.74,10*0./
c3	data bf/1.67,1.71,1.71,1.73,1.8,1.73,1.7,1.72,1.73,10*0./

cccccccccccc  S26 + 4n upper subshells cccccccccccccccccccccccccccccc
c      Data ln/13/,l/5,8,0/,alo/2,4,2,6,2,2,4,2,2,0,0,0,0,6*0/
c      data al/0.,1.,1.,2.,0.,0.,1.,1.,2.,0.,2.,3.,1.,6*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,6*0./
c1      data en/-45.5,-29.9,-22.0,-15.0,-10.0,-30.2,-22.,-11.,-4.,10*0./
c      data en/-24.2,-15.,-10.5,-5.,-2.,
c     *-40.,-29.,-25.,-18.,-16.,-12.,-7.,-4.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.88,1.86,1.84,1.78,1.74,
c     *1.65,1.67,1.7,1.7,1.74,1.74,1.76,1.76,6*0./

cccccccccccc  S26 + 3n upper subshells cccccccccccccccccccccccccccccc
c      Data ln/12/,l/5,7,0/,alo/2,4,2,6,0,2,4,2,2,0,0,0,28*0/
c      data al/0.,1.,1.,2.,0.,0.,1.,1.,2.,0.,2.,3.,28*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,28*0./
c1      data en/-45.5,-29.9,-22.0,-15.0,-10.0,-30.2,-22.,-11.,-4.,31*0./
c      data en/-24.2,-15.,-10.5,-5.,-2.,
c     *-40.,-29.,-25.,-18.,-16.,-12.,-7.,28*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.88,1.86,1.84,1.78,1.74,
c     *1.65,1.67,1.7,1.7,1.74,1.74,1.76,28*0./
cccccccccccc  S27L + 3n upper subshells cccccccccccccccccccccccccccccc
c      Data ln/13/,l/5,7,1/,alo/2,4,2,6,2,2,4,2,2,0,0,0,1,27*0/
c      data al/0.,1.,1.,2.,0.,0.,1.,1.,2.,0.,2.,3.,0.,27*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,0.5,27*0./
c      data en/-24.2,-15.,-10.5,-5.,-2.,
c     *-40.,-29.,-25.,-18.,-16.,-12.,-7.,-5,27*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.88,1.86,1.84,1.78,1.74,
c     *1.65,1.67,1.7,1.7,1.74,1.74,1.76,1.74,27*0./

cccccccccccc  S27 + 4n upper subshells  ccccccccccccccccccccccccccccc
c      Data ln/13/,l/5,8,0/,alo/2,4,2,6,2,2,4,2,3,0,0,0,0,27*0/
c      data al/0.,1.,1.,2.,0.,0.,1.,1.,2.,0.,2.,3.,1.,27*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,27*0./
c      data en/-45.5,-29.9,-22.0,-15.0,-10.0,-30.2,-22.,-11.,-4.,31*0./
c      data en/-25.5,-16.3,-11.9,-7.,-4.,
c     *-40.,-29.,-25.,-18.,-15.,-11.,-7.,-4,27*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.74,1.74,1.76,1.78,1.78,
c     *1.65,1.67,1.7,1.7,1.74,1.74,1.76,1.76,27*0./

cccccccccccc  S28 + 4n upper subshells (doesnt work for SkIII)  ccccccccccccccc
c      Data ln/13/,l/5,8,0/,alo/2,4,2,6,2,2,4,2,4,0,0,0,0,6*0/
c      data al/0.,1.,1.,2.,0.,0.,1.,1.,2.,0.,2.,3.,1.,6*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,6*0./
c1      data en/-45.5,-29.9,-22.0,-15.0,-10.0,-30.2,-22.,-11.,-4.,10*0./
c      data en/-27.6,-17.9,-13.,-7.,-4.,
c     *-39.,-28.,-26.,-17.,-15.,-10.,-6.,-4.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.65,1.7,1.74,1.76,1.78,
c     *1.65,1.67,1.7,1.7,1.74,1.74,1.76,1.76,6*0./

cccccccccccc  S28 (doesnt work for SkIII)  cccccccccccccccccccccccccccc
c      Data ln/9/,l/5,4,0/,alo/2,4,2,6,2,2,4,2,4,31*0/
c      data al/0.,1.,1.,2.,0.,0.,1.,1.,2.,31*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,0.5,1.5,0.5,2.5,31*0./
c      data en/-45.5,-29.9,-22.0,-15.0,-10.0,-30.2,-22.,-11.,-4.,31*0./
c      data en/-26.6,-17.7,-13.,-8.,-5.,-39.2,-28.,-24.,-18.,31*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.74,1.65,1.67,1.7,1.7,31*0./
c2      data bf/1.67,1.77,1.8,1.8,1.9,1.67,1.67,1.7,1.74,10*0./
c3	data bf/1.67,1.71,1.71,1.73,1.8,1.73,1.7,1.72,1.73,10*0./
cccccccccccc  S29L (doesnt work for SkIII)  cccccccccccccccccccccccccccc
c      Data ln/10/,l/5,4,1/,alo/2,4,2,6,2,2,4,2,4,1,30*0/
c      data al/0.,1.,1.,2.,0.,0.,1.,1.,2.,0.,30*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,0.5,1.5,0.5,2.5,0.5,30*0./
c      data en/-45.5,-29.9,-22.,-15.,-10.,-30.2,-22.,-11.,-4.,-10.,30*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.74,1.65,1.67,1.7,1.7,1.7,30*0./

cccccccccccc  S29 + 4n upper subshells (doesnt work for SkIII)  ccccccccccccccc
c      Data ln/13/,l/5,8,0/,alo/2,4,2,6,2,2,4,2,5,0,0,0,0,6*0/
c      data al/0.,1.,1.,2.,0.,0.,1.,1.,2.,0.,2.,3.,1.,6*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,6*0./
c1      data en/-45.5,-29.9,-22.0,-15.0,-10.0,-30.2,-22.,-11.,-4.,10*0./
c      data en/-27.6,-17.9,-13.,-7.,-4.,
c     *-39.,-28.,-26.,-17.,-15.,-10.,-6.,-3.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.76,
c     *1.65,1.67,1.7,1.7,1.74,1.74,1.76,1.76,6*0./
c2      data bf/1.67,1.77,1.8,1.8,1.9,1.67,1.67,1.7,1.74,10*0./
c3	data bf/1.67,1.71,1.71,1.73,1.8,1.73,1.7,1.72,1.73,10*0./

cccccccccccc  S29 (doesnt work for SkIII)  cccccccccccccccccccccccccccc
c      Data ln/9/,l/5,4,0/,alo/2,4,2,6,2,2,4,2,5,31*0/
c      data al/0.,1.,1.,2.,0.,0.,1.,1.,2.,31*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,0.5,1.5,0.5,2.5,31*0./
c      data en/-36.6,-23.9,-18.0,-10.0,-5.0,-44.2,-30.,-26.,-16.,31*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.74,1.65,1.67,1.7,1.7,31*0./
cccccccccccc  S30L (doesnt work for SkIII)  cccccccccccccccccccccccccccc
c      Data ln/10/,l/5,4,1/,alo/2,4,2,6,2,2,4,2,5,1,30*0/
c      data al/0.,1.,1.,2.,0.,0.,1.,1.,2.,0.,30*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,0.5,1.5,0.5,2.5,0.5,30*0./
c      data en/-36.6,-23.9,-18.,-10.,-5.,-44.2,-30.,-26.,-16.,-15.,30*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.74,1.65,1.67,1.7,1.7,1.7,30*0./

cccccccccccc  S30 + 1p 3n upper (doesnt work for SkIII)  ccccccccccccccc
c      Data ln/13/,l/6,7,0/,alo/2,4,2,6,2,0,2,4,2,6,0,0,0,27*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,3.,27*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,27*0./
c      data en/-36.6,-23.9,-18.,-10.,-5.,-1.,
c     *-44.2,-30.,-26.,-17.,-15.,-10.,-6.,27*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.74,1.74,
c     *1.65,1.67,1.7,1.7,1.74,1.74,1.76,27*0./

cccccccccccc  S31 + 1p 2n upper (doesnt work for SkIII)  ccccccccccccccc
c      Data ln/13/,l/6,7,0/,alo/2,4,2,6,2,0,2,4,2,6,1,0,0,27*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,3.,27*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,27*0./
c      data en/-36.6,-23.9,-18.,-10.,-5.,-1.,
c     *-44.2,-30.,-26.,-17.,-15.,-10.,-6.,27*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.74,1.74,
c     *1.65,1.67,1.7,1.7,1.74,1.74,1.76,27*0./

cccccccccccc  S32 + 1p 2n upper (doesnt work for SkIII)  ccccccccccccccc
c      Data ln/13/,l/6,7,0/,alo/2,4,2,6,2,0,2,4,2,6,2,0,0,27*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,3.,27*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,27*0./
c      data en/-36.6,-23.9,-18.,-10.,-5.,-1.,
c     *-44.2,-30.,-26.,-17.,-15.,-10.,-6.,27*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.74,1.74,
c     *1.65,1.67,1.7,1.7,1.74,1.74,1.76,27*0./

cccccccccccc  S32 (doesnt work for SkIII)  ccccccccccccccc
c      Data ln/10/,l/5,5,0/,alo/2,4,2,6,2,2,4,2,6,2,9*0/
c      data al/0.,1.,1.,2.,0.,0.,1.,1.,2.,0.,9*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,0.5,1.5,0.5,2.5,0.5,9*0./
c      data en/-36.6,-23.9,-18.,-10.,-5.,
c     *-44.2,-30.,-26.,-17.,-15.,9*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.74,
c     *1.65,1.67,1.7,1.7,1.74,9*0./

cccccccccccc  S33 + 1p 1n upper (doesnt work for SkIII)  ccccccccccccccc
c      Data ln/13/,l/6,7,0/,alo/2,4,2,6,2,0,2,4,2,6,2,1,0,27*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,3.,27*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,27*0./
c      data en/-36.6,-23.9,-18.,-10.,-5.,-1.,
c     *-44.2,-30.,-26.,-17.,-15.,-10.,-6.,27*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.74,1.74,
c     *1.65,1.67,1.7,1.7,1.74,1.74,1.76,27*0./

cccccccccccc  S34 + 1p 1n upper (doesnt work for SkIII)  ccccccccccccccc
c      Data ln/13/,l/6,7,0/,alo/2,4,2,6,2,0,2,4,2,6,2,2,0,27*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,3.,27*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,27*0./
c      data en/-36.6,-23.9,-18.,-10.,-5.,-1.,
c     *-44.2,-30.,-26.,-17.,-15.,-10.,-6.,27*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.74,1.74,
c     *1.65,1.67,1.7,1.7,1.74,1.74,1.76,27*0./

cccccccccccc  S35 + 2p 1n upper (c0 fot Skp c1 for Sly230b)  cccccccccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,2,0,0,2,4,2,6,2,3,0,26*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2.,0.,2.,3.,26*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,26*0./
c      data en/-42.,-28.,-23.,-14.,-10.,-7.,-4.,
c     *-47.2,-32.,-27.,-18.,-13.,-10.,-5.,26*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.74,1.74,1.76,
c     *1.67,1.7,1.72,1.72,1.74,1.76,1.78,26*0./

cccccccccccc  S36 + 2p 1n upper   ccccccccccccccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,2,0,0,2,4,2,6,2,4,0,26*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,
c     *0.,1.,1.,2.,0.,2.,3.,26*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,26*0./
c      data en/-43.,-29.,-24.,-16.,-11.,-8.,-3.,
c     *-47.,-32.,-27.,-18.,-13.,-10.,-5.,26*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.74,1.74,1.76,
c     *1.67,1.7,1.72,1.72,1.74,1.76,1.78,26*0./

cccccccccccc  S37 + 2p upper   ccccccccccccccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,2,0,0,2,4,2,6,2,4,1,26*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,
c     *0.,1.,1.,2.,0.,2.,3.,26*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,26*0./
c      data en/-33.2,-23.9,-20.,-14.,-10.,-9.,-4.,
c     *-34.2,-26.,-22.,-16.,-13.,-10.,-6.,26*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.74,1.74,1.76,
c     *1.65,1.67,1.7,1.7,1.74,1.74,1.76,26*0./

cccccccccccc  S38 + 2p upper   ccccccccccccccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,2,0,0,2,4,2,6,2,4,2,26*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,
c     *0.,1.,1.,2.,0.,2.,3.,26*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,26*0./
c      data en/-36.,-27.,-24.,-18.,-13.,-12.,-7.,
c     *-34.,-25.,-21.,-15.,-13.,-10.,-6.,26*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.74,1.74,1.76,
c     *1.65,1.67,1.7,1.7,1.74,1.74,1.76,26*0./
cccccccccccc  S39L + 2p upper   ccccccccccccccc
c      Data ln/15/,l/7,7,1/,alo/2,4,2,6,2,0,0,2,4,2,6,2,4,2,1,25*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,
c     *0.,1.,1.,2.,0.,2.,3.,0.,25*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,0.5,25*0./
c      data en/-36.,-27.,-24.,-18.,-13.,-12.,-7.,
c     *-34.,-25.,-21.,-15.,-13.,-10.,-6.,-10.,25*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.74,1.74,1.76,
c     *1.65,1.67,1.7,1.7,1.74,1.74,1.76,1.76,25*0./


cccccccccccc  S39 + 2p upper   cccccccccccccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,2,0,0,2,4,2,6,2,4,3,26*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,
c     *0.,1.,1.,2.,0.,2.,3.,26*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,26*0./
c      data en/-45.,-32.,-27.,-19.,-14.,-12.,-6.,
c     *-47.,-32.,-28.,-18.,-14.,-11.,-6.,26*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.68,1.74,1.76,1.76,1.76,1.78,1.78,
c     *1.65,1.67,1.7,1.7,1.74,1.74,1.76,26*0./

cccccccccccc  S40 + 2p upper   ccccccccccccccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,2,0,0,2,4,2,6,2,4,4,26*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,
c     *0.,1.,1.,2.,0.,2.,3.,26*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,26*0./
c      data en/-37.,-28.,-25.,-19.,-14.,-13.,-8.,
c     *-34.2,-26.,-22.,-16.,-13.,-10.,-6.,26*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.74,1.74,1.76,
c     *1.65,1.67,1.7,1.7,1.74,1.74,1.76,26*0./

cccccccccccc  S41 + 2p upper (doesnt work for SkIII)  ccccccccccccccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,2,0,0,2,4,2,6,2,4,5,26*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,
c     *0.,1.,1.,2.,0.,2.,3.,26*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,26*0./
c      data en/-37.,-28.,-25.,-19.,-14.,-13.,-8.,
c     *-34.2,-26.,-22.,-16.,-13.,-10.,-6.,26*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.74,1.74,1.76,
c     *1.65,1.67,1.7,1.7,1.74,1.74,1.76,26*0./

cccccccccccc  S42 + 3p upper (doesnt work for SkIII)  ccccccccccccccc
c      Data ln/15/,l/8,7,0/,alo/2,4,2,6,2,0,0,0,2,4,2,6,2,4,6,25*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,
c     *0.,1.,1.,2.,0.,2.,3.,25*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,25*0./
c      data en/-37.,-28.,-25.,-19.,-14.,-13.,-8.,-3.,
c     *-34.2,-26.,-22.,-16.,-13.,-10.,-6.,25*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.74,1.74,1.76,1.76,
c     *1.65,1.67,1.7,1.7,1.74,1.74,1.76,25*0./

cccccccccccc  S43 + 3p upper (doesnt work for SkIII)  ccccccccccccccc
c      Data ln/15/,l/8,7,0/,alo/2,4,2,6,2,0,0,0,2,4,2,6,2,4,7,25*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,
c     *0.,1.,1.,2.,0.,2.,3.,25*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,25*0./
c      data en/-37.,-28.,-25.,-19.,-14.,-13.,-8.,-3.,
c     *-34.2,-26.,-22.,-16.,-13.,-10.,-6.,25*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.74,1.74,1.76,1.76,
c     *1.65,1.67,1.7,1.7,1.74,1.74,1.76,25*0./

cccccccccccc  S44 + 3p upper (c0 for Skp c1 for Sly230b)  ccccccccccccccc
c      Data ln/15/,l/8,7,0/,alo/2,4,2,6,2,0,0,0,2,4,2,6,2,4,8,25*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,
c     *0.,1.,1.,2.,0.,2.,3.,25*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,25*0./
c      data en/-49.,-36.,-32.,-23.,-18.,-17.,-11.,-6.,
c     *-48.,-33.,-30.,-19.,-14.,-12.,-6.,25*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.68,1.74,1.76,1.76,1.76,1.78,1.78,1.78,
c     *1.65,1.67,1.7,1.7,1.74,1.74,1.76,25*0./

cccccccccccc  S45 + 4p upper (doesnt work for SkIII)  ccccccccccccccc
c      Data ln/17/,l/9,8,0/,alo/2,4,2,6,2,0,0,0,0,2,4,2,6,2,4,8,1,2*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,3.,
c     *0.,1.,1.,2.,0.,2.,3.,1.,2*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,2.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,2*0./
c      data en/-41.,-33.,-30.,-23.,-18.,-18.,-13.,-8.,-4.,
c     *-33.,-24.,-21.,-15.,-12.,-10.,-6.,-4.,2*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.74,1.74,1.76,1.76,1.76,
c     *1.65,1.67,1.7,1.7,1.74,1.74,1.76,1.76,2*0./

cccccccccccc  S46 + 4p upper (doesnt work for SkIII)  ccccccccccccccc
c      Data ln/17/,l/9,8,0/,alo/2,4,2,6,2,0,0,0,0,2,4,2,6,2,4,8,2,2*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,3.,
c     *0.,1.,1.,2.,0.,2.,3.,1.,2*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,2.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,2*0./
c      data en/-41.,-33.,-30.,-23.,-18.,-18.,-13.,-8.,-4.,
c     *-33.,-24.,-21.,-15.,-12.,-10.,-6.,-4.,2*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.74,1.74,1.76,1.76,1.78,
c     *1.64,1.68,1.7,1.7,1.74,1.74,1.74,1.74,2*0./

cccccccccccc  S47 + 4p upper (doesnt work for SkIII)  ccccccccccccccc
c      Data ln/17/,l/9,8,0/,alo/2,4,2,6,2,0,0,0,0,2,4,2,6,2,4,8,3,2*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,3.,
c     *0.,1.,1.,2.,0.,2.,3.,1.,2*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,2.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,2*0./
c      data en/-41.,-33.,-30.,-23.,-18.,-18.,-13.,-8.,-4.,
c     *-33.,-24.,-21.,-15.,-12.,-10.,-6.,-4.,2*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.74,1.74,1.76,1.76,1.78,
c     *1.72,1.72,1.72,1.72,1.74,1.74,1.74,1.74,2*0./

cccccccccccc  S48 + 4p upper (doesnt work for SkIII)  ccccccccccccccc
c      Data ln/17/,l/9,8,0/,alo/2,4,2,6,2,0,0,0,0,2,4,2,6,2,4,8,4,2*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,3.,
c     *0.,1.,1.,2.,0.,2.,3.,1.,2*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,2.5,
c     *0.5,1.5,0.5,2.5,0.5,1.5,3.5,1.5,2*0./
c      data en/-41.,-33.,-30.,-23.,-18.,-18.,-13.,-8.,-4.,
c     *-33.,-24.,-21.,-15.,-12.,-10.,-6.,-4.,2*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.74,1.74,1.76,1.76,1.78,
c     *1.76,1.76,1.76,1.74,1.74,1.74,1.74,1.74,2*0./

cccccccccccc  Cl30  ccccccccccccccccccccccccccccccccccccc
c      Data ln/10/,l/6,4,0/,alo/2,4,2,6,2,1,2,4,2,5,30*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,30*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,1.5,0.5,2.5,30*0./
c      data en/-40.5,-23.9,-18.,-9.5,-6.,-1.,
c     *-49.2,-34.,-29.,-19.,30*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.65,1.7,1.77,1.74,1.8,1.8,1.65,1.67,1.7,1.74,30*0./

cccccccccccc 17 Cl30 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/10/,l/6,4,0/,alo/2,4,2,6,2,1,2,4,2,5,30*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,30*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,0.5,1.5,0.5,2.5,30*0./
c      data en/-38.,-22.5,-17.,-8.,-5,-1.,
c     *-49.,-34.,-29.,-19.5,30*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/2.6,2.4,2.3,2.1,2.,1.9,
c     *2.6,2.4,2.4,2.1,30*0./
cccccccccccc 17 Cl31L cccccccccccccccccccccccccccccccccccccccc
c      Data ln/11/,l/6,4,1/,alo/2,4,2,6,2,1,2,4,2,5,1,29*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,29*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,0.5,1.5,0.5,2.5,0.5,29*0./
c      data en/-38.,-22.5,-17.,-8.,-5,-1.,
c     *-49.,-34.,-29.,-20.,-15.,29*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/2.6,2.4,2.3,2.1,2.,1.9,
c     *2.6,2.4,2.4,2.1,2.,29*0./

cccccccccccc  Cl31  ccccccccccccccccccccccccccccccccccccc
c      Data ln/10/,l/6,4,0/,alo/2,4,2,6,2,1,2,4,2,6,30*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,30*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,1.5,0.5,2.5,30*0./
c      data en/-40.5,-24.9,-18.,-9.,-6.,-1.,
c     *-48.2,-32.,-27.,-18.,30*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.65,1.7,1.77,1.74,1.8,1.8,1.65,1.67,1.7,1.74,30*0./

cccccccccccc  Cl33  ccccccccccccccccccccccccccccccccccccc
c      Data ln/10/,l/6,5,0/,alo/2,4,2,6,2,1,2,4,2,6,2,29*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,29*0./
c      data aj/0.5,1.5,0.5,2.5,0.5,1.5,0.5,1.5,0.5,2.5,0.5,29*0./
c      data en/-40.5,-24.9,-18.,-9.,-6.,-3.,
c     *-48.2,-32.,-27.,-18.,-15.,29*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/1.65,1.7,1.77,1.74,1.8,1.8,1.65,1.67,1.7,1.74,1.74,29*0./

cccccccccccc 18 Ar30 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/10/,l/6,4,0/,alo/2,4,2,6,2,2,2,4,2,4,30*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,30*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,0.5,1.5,0.5,2.5,30*0./
c      data en/-36.,-20.5,-15.5,-7.,-3.5,-1.,
c     *-49.,-34.5,-30.,-21.,30*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/2.6,2.4,2.3,2.1,2.,1.9,
c     *2.6,2.4,2.4,2.1,30*0./
cccccccccccc 18 Ar31L cccccccccccccccccccccccccccccccccccccccc
c      Data ln/11/,l/6,4,1/,alo/2,4,2,6,2,2,2,4,2,4,1,29*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,29*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,0.5,1.5,0.5,2.5,0.5,29*0./
c      data en/-36.,-20.5,-15.5,-7.,-3.5,-1.,
c     *-49.,-34.5,-30.,-21.,-10.,29*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/2.6,2.4,2.3,2.1,2.,1.9,
c     *2.6,2.4,2.4,2.1,2.,29*0./

cccccccccccc 18 Ar32 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/10/,l/6,4,0/,alo/2,4,2,6,2,2,2,4,2,6,30*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,30*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,0.5,1.5,0.5,2.5,30*0./
c      data en/-36.,-20.5,-15.5,-7.,-3.5,-1.,
c     *-49.,-34.5,-30.,-21.,30*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/2.6,2.4,2.3,2.1,2.,1.9,
c     *2.6,2.4,2.4,2.1,30*0./
cccccccccccc 18 Ar33L cccccccccccccccccccccccccccccccccccccccc
c      Data ln/11/,l/6,4,1/,alo/2,4,2,6,2,2,2,4,2,6,1,29*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,29*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,0.5,1.5,0.5,2.5,0.5,29*0./
c      data en/-36.,-20.5,-15.5,-7.,-3.5,-1.,
c     *-49.,-34.5,-30.,-21.,-15.,29*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/2.6,2.4,2.3,2.1,2.,1.9,
c     *2.6,2.4,2.4,2.1,2.,29*0./

cccccccccccc 18 Ar33 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/11/,l/6,5,0/,alo/2,4,2,6,2,2,2,4,2,6,1,29*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,29*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,0.5,1.5,0.5,2.5,0.5,29*0./
c      data en/-38.,-23.,-18.,-9.,-6.,-2.,
c     *-50.,-35.,-30.,-21.,-18.,29*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/2.6,2.4,2.3,2.1,2.,1.9,
c     *2.6,2.4,2.4,2.1,2.3,29*0./
cccccccccccc 18 Ar34L cccccccccccccccccccccccccccccccccccccccc
c      Data ln/12/,l/6,5,1/,alo/2,4,2,6,2,2,2,4,2,6,1,1,28*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,0.,28*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,0.5,1.5,0.5,2.5,0.5,0.5,28*0./
c      data en/-38.,-23.,-18.,-9.,-6.,-2.,
c     *-50.,-35.,-30.,-21.,-18.,-15.,28*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/2.6,2.4,2.3,2.1,2.,1.9,
c     *2.6,2.4,2.4,2.1,2.3,2.3,28*0./


cccccccccccc 19 K34 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/11/,l/6,5,0/,alo/2,4,2,6,2,3,2,4,2,6,1,29*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,29*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,0.5,1.5,0.5,2.5,0.5,29*0./
c      data en/-38.,-23.,-18.,-9.,-6.,-2.,
c     *-50.,-35.,-30.,-21.,-18.,29*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/2.6,2.4,2.3,2.1,2.,1.9,
c     *2.6,2.4,2.4,2.1,2.3,29*0./
cccccccccccc 19 K35L cccccccccccccccccccccccccccccccccccccccc
c      Data ln/12/,l/6,5,1/,alo/2,4,2,6,2,3,2,4,2,6,1,1,28*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,0.,28*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,0.5,1.5,0.5,2.5,0.5,0.5,28*0./
c      data en/-38.,-23.,-18.,-9.,-6.,-2.,
c     *-50.,-35.,-30.,-21.,-18.,-15.,28*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/2.6,2.4,2.3,2.1,2.,1.9,
c     *2.6,2.4,2.4,2.1,2.3,2.3,28*0./

cccccccccccc 20 Ca34 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/10/,l/6,4,0/,alo/2,4,2,6,2,4,2,4,2,6,30*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,30*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,0.5,1.5,0.5,2.5,30*0./
c      data en/-38.,-23.,-18.,-9.,-6.,-2.,
c     *-50.,-35.,-30.,-21.,30*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/2.6,2.4,2.3,2.1,2.,1.9,
c     *2.6,2.4,2.4,2.1,30*0./
cccccccccccc 20 Ca35L cccccccccccccccccccccccccccccccccccccccc
c      Data ln/11/,l/6,4,1/,alo/2,4,2,6,2,4,2,4,2,6,1,29*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,29*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,0.5,1.5,0.5,2.5,0.5,29*0./
c      data en/-38.,-23.,-18.,-9.,-6.,-2.,
c     *-50.,-35.,-30.,-21.,-20.,29*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/2.6,2.4,2.3,2.1,2.,1.9,
c     *2.6,2.4,2.4,2.1,2.1,29*0./

cccccccccccc 20 Ca35 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/11/,l/6,5,0/,alo/2,4,2,6,2,4,2,4,2,6,1,29*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,29*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,0.5,1.5,0.5,2.5,0.5,29*0./
c      data en/-38.,-23.,-18.,-9.,-6.,-2.,
c     *-50.,-35.,-30.,-21.,-18.,29*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/2.6,2.4,2.3,2.1,2.,1.9,
c     *2.6,2.4,2.4,2.1,2.3,29*0./
cccccccccccc 20 Ca36L cccccccccccccccccccccccccccccccccccccccc
c      Data ln/12/,l/6,5,1/,alo/2,4,2,6,2,4,2,4,2,6,1,1,28*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,0.,28*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,0.5,1.5,0.5,2.5,0.5,0.5,28*0./
c      data en/-38.,-23.,-18.,-9.,-6.,-2.,
c     *-50.,-35.,-30.,-21.,-18.,-20.,28*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/2.6,2.4,2.3,2.1,2.,1.9,
c     *2.6,2.4,2.4,2.1,2.3,2.3,28*0./


cccccccccccc B10L cccccccccccccccccccccccccccccccccccccccc
c      Data ln/5/,l/2,2,1/,alo/2,3,2,2,1,0,7*0,6*0/
c      data al/0.,1.,0.,1.,1.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,1.5,0.,0.,6*0.,6*0./
c      data en/-26.5,-11.9,-34.2,-17.,-5.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.6,1.74,1.9,0.,0.,12*0./

cccccccccccc Ne23 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/8/,l/4,4,0/,alo/2,4,2,2,2,4,2,5,5*0,6*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,5*0.,6*0./
c      data aj/.5,1.5,.5,2.5,0.5,1.5,0.5,2.5,5*0.,6*0./
c      data en/-36.5,-21.9,-14.2,-7.,-36.5,-21.9,-14.2,-7.,5*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.62,1.7,1.7,1.74,11*0./

cccccccccccc Si26 ccccccZ=14ccccN=12cccccccccccccccccccccccccccccc
c      Data ln/11/,l/4,7,0/,alo/2,4,2,6,2,4,2,6,2,4,0,0,7*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,2.,3.,8*0./
c      data aj/.5,1.5,.5,2.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,8*0./
c      data en/-28.,-17.,-14.,-7.,
c     *-36.,-26.,-22.,-14.,-9.,-6.,-1.,8*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.65,1.67,1.7,1.7,1.75,1.77,1.8,8*0./


ccccccccccccccc//////// N = 20 //////////////////////cccccccccccccccccccc

cccccccccccc 8 O28 (c3 for Skp&SkIII&) ccccccccccccccccccccccccc
c      Data ln/9/,l/3,6,0/,alo/2,4,2,2,4,2,6,2,4,10*0/
c      data al/0.,1.,1.,0.,1.,1.,2.,0.,2.,10*0./
c      data aj/.5,1.5,.5,0.5,1.5,0.5,2.5,0.5,1.5,10*0./
c      data en/-41.,-31.,-28.,
c     *-26.,-17.,-13.,-8.,-6.,-2.,10*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c1      data bf/1.62,1.7,1.7,1.65,1.67,1.7,1.7,1.75,1.77,10*0./
c2      data bf/1.62,1.7,1.7,1.60,1.64,1.68,1.73,1.76,1.8,10*0./
c3      data bf/1.8,1.82,1.88,1.9,1.8,1.86,1.88,1.92,1.96,2.,9*0./

cccccccccccc 9 F29 (Skp) cccccccccccccccccccccccccccccccccc
c      Data ln/10/,l/4,6,0/,alo/2,4,2,1,2,4,2,6,2,4,9*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,2.,9*0./
c      data aj/.5,1.5,.5,2.5,0.5,1.5,0.5,2.5,0.5,1.5,9*0./
c1      data en/-40.,-29.,-26.,-19.,
c1     *-29.,-20.,-16.,-10.,-8.,-5.,9*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c1      data bf/1.8,1.82,1.88,1.9,1.8,1.86,1.88,1.92,1.96,2.,9*0./
c2      data bf/1.62,1.7,1.7,1.74,1.60,1.64,1.68,1.73,1.76,1.8,9*0./

cccccccccccc 9 F29 (SkIII&SLy4) cccccccccccccccccccccccccccccccccc
c      Data ln/10/,l/4,6,0/,alo/2,4,2,1,2,4,2,6,2,4,9*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,2.,9*0./
c      data aj/.5,1.5,.5,2.5,0.5,1.5,0.5,2.5,0.5,1.5,9*0./
c      data en/-46.,-33.,-29.,-20.,
c     *-36.,-23.,-19.,-11.,-7.,-4.,9*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.8,1.82,1.88,1.9,1.8,1.86,1.88,1.92,1.96,2.,9*0./

cccccccccccc 10 Ne30 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/10/,l/4,6,0/,alo/2,4,2,2,2,4,2,6,2,4,9*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,2.,9*0./
c      data aj/.5,1.5,.5,2.5,0.5,1.5,0.5,2.5,0.5,1.5,9*0./
c      data en/-40.,-29.,-26.,-19.,
c     *-29.,-20.,-16.,-10.,-8.,-5.,9*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c1      data bf/1.62,1.7,1.7,1.74,1.65,1.67,1.7,1.7,1.75,1.77,9*0./
c2      data bf/1.62,1.7,1.7,1.74,1.60,1.64,1.68,1.73,1.76,1.8,9*0./
c3      data bf/1.8,1.82,1.88,1.9,1.8,1.86,1.88,1.92,1.96,2.,9*0./

cccccccccccc 11 Na31 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/10/,l/4,6,0/,alo/2,4,2,3,2,4,2,6,2,4,9*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,2.,9*0./
c      data aj/.5,1.5,.5,2.5,0.5,1.5,0.5,2.5,0.5,1.5,9*0./
c1      data en/-45.,-31.,-27.,-18.,
c1     *-42.,-26.,-22.,-14.,-9.,-6.,9*0./
c3      data en/-43.,-30.,-26.,-18.,
c3     *-37.,-25.,-20.,-13.,-10.,-6.,9*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c1      data bf/1.62,1.7,1.7,1.74,1.60,1.64,1.68,1.73,1.76,1.8,9*0./
c2      data en/-38.,-28.,-25.,-18.,
c2     *-29.,-20.,-17.,-11.,-8.,-5.,9*0./
c2      data bf/1.67,1.7,1.74,1.82,1.65,1.7,1.77,1.74,1.8,1.82,9*0./

cccccccccccc 12 Mg32 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/10/,l/4,6,0/,alo/2,4,2,4,2,4,2,6,2,4,9*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,2.,9*0./
c      data aj/.5,1.5,.5,2.5,0.5,1.5,0.5,2.5,0.5,1.5,9*0./
c1      data en/-44.,-30.,-27.,-18.,
c1     *-36.,-26.,-22.,-14.,-9.,-7.,9*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c2      data en/-28.,-17.,-14.,-7.,
c2     *-36.,-26.,-22.,-14.,-9.,-6.,9*0./
c1      data bf/1.62,1.7,1.7,1.74,1.65,1.67,1.7,1.7,1.75,1.77,9*0./
c2      data bf/1.65,1.67,1.7,1.74,1.65,1.7,1.77,1.74,1.8,1.82,9*0./

cccccccccccc 12 Mg32 (SkIII&SLy4) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/10/,l/4,6,0/,alo/2,4,2,4,2,4,2,6,2,4,9*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,2.,9*0./
c      data aj/.5,1.5,.5,2.5,0.5,1.5,0.5,2.5,0.5,1.5,9*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data en/-44.,-30.,-27.,-18.,
c     *-39.,-27.,-23.,-15.,-10.,-7.,9*0./
c      data bf/1.67,1.7,1.74,1.82,1.65,1.7,1.77,1.8,1.82,1.84,9*0./

cccccccccccc 13 Al33 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/10/,l/4,6,0/,alo/2,4,2,5,2,4,2,6,2,4,9*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,2.,9*0./
c      data aj/.5,1.5,.5,2.5,0.5,1.5,0.5,2.5,0.5,1.5,9*0./
c      data en/-28.,-17.,-14.,-7.,
c     *-36.,-26.,-22.,-14.,-9.,-6.,9*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.65,1.67,1.7,1.7,1.75,1.77,9*0./

cccccccccccc 14 Si34 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/10/,l/4,6,0/,alo/2,4,2,6,2,4,2,6,2,4,9*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,2.,9*0./
c      data aj/.5,1.5,.5,2.5,0.5,1.5,0.5,2.5,0.5,1.5,9*0./
c      data en/-28.,-17.,-14.,-7.,
c     *-36.,-26.,-22.,-14.,-9.,-6.,9*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.65,1.67,1.7,1.7,1.75,1.77,9*0./

cccccccccccc 15 P35 (c2 for SkIII) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/11/,l/5,6,0/,alo/2,4,2,6,1,2,4,2,6,2,4,8*0/
c      data al/0.,1.,1.,2.,0.,0.,1.,1.,2.,0.,2.,8*0./
c      data aj/.5,1.5,.5,2.5,0.5,0.5,1.5,0.5,2.5,0.5,1.5,8*0./
c1      data en/-28.,-17.,-14.,-7.,-5.,
c1     *-36.,-26.,-22.,-14.,-9.,-6.,8*0./
c2      data en/-43.,-30.,-26.,-17.,-10.,
c2     *-40.,-26.,-22.,-14.,-9.,-6.,8*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c1      data bf/1.62,1.7,1.7,1.74,1.77,
c1     *1.65,1.67,1.7,1.7,1.75,1.8,8*0./
c3      data bf/1.62,1.7,1.7,1.74,1.77,
c3     *1.60,1.64,1.68,1.73,1.76,1.8,8*0./
c4      data bf/1.62,1.7,1.7,1.74,1.78,
c4     *1.65,1.67,1.7,1.7,1.75,1.77,8*0./
c2	data bf/2.6,2.4,2.3,2.1,2.,
c2     *2.6,2.4,2.4,2.1,2.1,1.9,8*0./

cccccccccccc 16 S36 (c2 for SkIII) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/11/,l/5,6,0/,alo/2,4,2,6,2,2,4,2,6,2,4,8*0/
c      data al/0.,1.,1.,2.,0.,0.,1.,1.,2.,0.,2.,8*0./
c      data aj/.5,1.5,.5,2.5,0.5,0.5,1.5,0.5,2.5,0.5,1.5,8*0./
c1      data en/-28.,-17.,-14.,-7.,-5.,
c1     *-36.,-26.,-22.,-14.,-9.,-6.,8*0./
c2      data en/-43.,-30.,-26.,-17.,-10.,
c2     *-40.,-26.,-22.,-14.,-9.,-6.,8*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c1      data bf/1.62,1.7,1.7,1.74,1.77,
c1     *1.65,1.67,1.7,1.7,1.75,1.8,8*0./
c2	data bf/2.6,2.4,2.3,2.1,2.,
c2     *2.6,2.4,2.4,2.1,2.1,1.9,8*0./

cccccccccccc 17 Cl37(SkIII) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/12/,l/6,6,0/,alo/2,4,2,6,2,1,2,4,2,6,2,4,7*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,7*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,0.5,1.5,0.5,2.5,0.5,1.5,7*0./
c      data en/-43.,-30.,-26.,-17.,-10.,-5.,
c     *-40.,-26.,-22.,-14.,-9.,-6.,7*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/2.6,2.4,2.3,2.1,2.,1.9,
c     *2.6,2.4,2.4,2.1,2.1,1.9,7*0./

cccccccccccc 18 Ar38(SkIII) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/12/,l/6,6,0/,alo/2,4,2,6,2,2,2,4,2,6,2,4,7*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,7*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,0.5,1.5,0.5,2.5,0.5,1.5,7*0./
c      data en/-43.,-30.,-26.,-17.,-10.,-5.,
c     *-40.,-26.,-22.,-14.,-9.,-6.,7*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/2.6,2.4,2.3,2.1,2.,1.9,
c     *2.6,2.4,2.4,2.1,2.1,1.9,7*0./

cccccccccccc 19 K39(SkIII) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/12/,l/6,6,0/,alo/2,4,2,6,2,3,2,4,2,6,2,4,7*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,7*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,0.5,1.5,0.5,2.5,0.5,1.5,7*0./
c      data en/-43.,-30.,-26.,-17.,-10.,-5.,
c     *-40.,-26.,-22.,-14.,-9.,-6.,7*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/2.6,2.4,2.3,2.1,2.,1.9,
c     *2.6,2.4,2.4,2.1,2.1,1.9,7*0./

cccccccccccc 20 40(SkIII) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/12/,l/6,6,0/,alo/2,4,2,6,2,4,2,4,2,6,2,4,28*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,28*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,0.5,1.5,0.5,2.5,0.5,1.5,28*0./
c      data en/-43.,-30.,-26.,-17.,-10.,-5.,
c     *-40.,-26.,-22.,-14.,-9.,-6.,28*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c	data bf/2.6,2.4,2.3,2.1,2.,1.9,
c     *2.6,2.4,2.4,2.1,2.1,1.9,28*0./

cccccccccccc 21 Sc41(SkIII) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/13/,l/7,6,0/,alo/2,4,2,6,2,4,1,2,4,2,6,2,4,6*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2.,0.,2.,6*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,0.5,1.5,0.5,2.5,0.5,1.5,6*0./
c      data en/-36.,-26.,-22.,-14.,-8.,-8.,-4.,
c     *-44.,-33.,-30.,-21.,-15.,-15.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/2.6,2.4,2.3,2.1,2.,1.9,1.8,
c     *2.6,2.4,2.4,2.1,2.1,1.9,6*0./

cccccccccccc 22 Ti42(SkIII) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/13/,l/7,6,0/,alo/2,4,2,6,2,4,2,2,4,2,6,2,4,6*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2.,0.,2.,6*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,0.5,1.5,0.5,2.5,0.5,1.5,6*0./
c      data en/-36.,-26.,-22.,-14.,-8.,-8.,-4.,
c     *-44.,-33.,-30.,-21.,-15.,-15.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/2.6,2.4,2.3,2.1,2.,1.9,1.8,
c     *2.6,2.4,2.4,2.1,2.1,1.9,6*0./

cccccccccccc 23 V43(SkIII) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/13/,l/7,6,0/,alo/2,4,2,6,2,4,3,2,4,2,6,2,4,6*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2.,0.,2.,6*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,0.5,1.5,0.5,2.5,0.5,1.5,6*0./
c      data en/-36.,-26.,-22.,-14.,-8.,-8.,-4.,
c     *-44.,-33.,-30.,-21.,-15.,-15.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/2.6,2.4,2.3,2.1,2.,1.9,1.8,
c     *2.6,2.4,2.4,2.1,2.1,1.9,6*0./

cccccccccccc 24 Cr44(SkIII) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/13/,l/7,6,0/,alo/2,4,2,6,2,4,4,2,4,2,6,2,4,6*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2.,0.,2.,6*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,0.5,1.5,0.5,2.5,0.5,1.5,6*0./
c      data en/-36.,-26.,-22.,-14.,-8.,-8.,-4.,
c     *-44.,-33.,-30.,-21.,-15.,-15.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/2.6,2.4,2.3,2.1,2.,1.9,1.8,
c     *2.6,2.4,2.4,2.1,2.1,1.9,6*0./

cccccccccccc 25 Mn45(SkIII) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/13/,l/7,6,0/,alo/2,4,2,6,2,4,5,2,4,2,6,2,4,6*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2.,0.,2.,6*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,0.5,1.5,0.5,2.5,0.5,1.5,6*0./
c      data en/-36.,-26.,-22.,-14.,-8.,-8.,-4.,
c     *-44.,-33.,-30.,-21.,-15.,-15.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/2.6,2.4,2.3,2.1,2.,1.9,1.8,
c     *2.6,2.4,2.4,2.1,2.1,1.9,6*0./

cccccccccccc 25 Mn45(Skp) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/13/,l/7,6,0/,alo/2,4,2,6,2,4,5,2,4,2,6,2,4,6*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2.,0.,2.,6*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,0.5,1.5,0.5,2.5,0.5,1.5,6*0./
c      data en/-28.,-20.,-17.,-11.,-8.,-6.,-1.,
c     *-40.,-32.,-30.,-23.,-20.,-18.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/2.6,2.4,2.3,2.1,2.,1.9,1.8,
c     *2.6,2.4,2.4,2.1,2.1,1.9,6*0./

cccccccccccc 26 Fe46(SkIII) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/13/,l/7,6,0/,alo/2,4,2,6,2,4,6,2,4,2,6,2,4,6*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2.,0.,2.,6*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,0.5,1.5,0.5,2.5,0.5,1.5,6*0./
c      data en/-36.,-26.,-22.,-14.,-8.,-8.,-4.,
c     *-44.,-33.,-30.,-21.,-15.,-15.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/2.6,2.4,2.3,2.1,2.,1.9,1.8,
c     *2.6,2.4,2.4,2.1,2.1,1.9,6*0./

cccccccccccc 26 Fe46(Skp) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/13/,l/7,6,0/,alo/2,4,2,6,2,4,6,2,4,2,6,2,4,6*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2.,0.,2.,6*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,0.5,1.5,0.5,2.5,0.5,1.5,6*0./
c      data en/-27.,-19.,-17.,-10.,-8.,-6.,-1.,
c     *-42.,-33.,-30.,-24.,-20.,-19.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/2.6,2.4,2.3,2.1,2.,1.9,1.8,
c     *2.6,2.4,2.4,2.1,2.1,1.9,6*0./

cccccccccccc 27 Co47(SkIII) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/13/,l/7,6,0/,alo/2,4,2,6,2,4,7,2,4,2,6,2,4,6*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2.,0.,2.,6*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,0.5,1.5,0.5,2.5,0.5,1.5,6*0./
c      data en/-36.,-26.,-22.,-14.,-8.,-8.,-4.,
c     *-44.,-33.,-30.,-21.,-15.,-15.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/2.6,2.4,2.3,2.1,2.,1.9,1.8,
c     *2.6,2.4,2.4,2.1,2.1,1.9,6*0./

cccccccccccc 27 Co47(Skp) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/13/,l/7,6,0/,alo/2,4,2,6,2,4,7,2,4,2,6,2,4,6*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2.,0.,2.,6*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,0.5,1.5,0.5,2.5,0.5,1.5,6*0./
c      data en/-27.,-19.,-16.,-10.,-7.,-5.,-1.,
c     *-42.,-34.,-31.,-25.,-21.,-20.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/2.6,2.4,2.3,2.1,2.,1.9,1.8,
c     *2.6,2.4,2.4,2.1,2.1,1.9,6*0./

cccccccccccc 28 Ni48&&(SkIII) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/13/,l/7,6,0/,alo/2,4,2,6,2,4,8,2,4,2,6,2,4,6*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2.,0.,2.,6*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,0.5,1.5,0.5,2.5,0.5,1.5,6*0./
c      data en/-36.,-26.,-22.,-14.,-8.,-8.,-4.,
c     *-49.,-39.,-36.,-28.,-21.,-22.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/2.6,2.4,2.3,2.1,2.,1.9,1.8,
c     *2.6,2.4,2.4,2.1,2.1,1.9,6*0./

cccccccccccc 28 Ni48&&(Skp) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/13/,l/7,6,0/,alo/2,4,2,6,2,4,8,2,4,2,6,2,4,6*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2.,0.,2.,6*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,0.5,1.5,0.5,2.5,0.5,1.5,6*0./
c      data en/-27.,-18.,-16.,-9.,-7.,-5.,-1.,
c     *-43.,-35.,-32.,-26.,-22.,-21.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/2.6,2.4,2.3,2.1,2.,1.9,1.8,
c     *2.6,2.4,2.4,2.1,2.1,1.9,6*0./

cccccccccccc 28 Ni48&&(SLy4) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/13/,l/7,6,0/,alo/2,4,2,6,2,4,8,2,4,2,6,2,4,6*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2.,0.,2.,6*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,0.5,1.5,0.5,2.5,0.5,1.5,6*0./
c      data en/-39.,-26.,-23.,-13.,-8.,-7.,-1.,
c     *-53.,-40.,-37.,-28.,-23.,-22.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/2.6,2.4,2.3,2.1,2.,1.9,1.8,
c     *2.6,2.4,2.4,2.1,2.1,1.9,6*0./

cccccccccccc 28 Ni48&&(T22) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/13/,l/7,6,0/,alo/2,4,2,6,4,2,8,2,4,2,6,4,2,27*0/
c      data al/0.,1.,1.,2.,2.,0.,3.,0.,1.,1.,2.,2.,0.,27*0./
c      data aj/.5,1.5,.5,2.5,1.5,.5,3.5,0.5,1.5,0.5,2.5,1.5,.5,27*0./
c      data en/-39.,-26.,-23.,-13.,-8.,-7.,-1.,
c     *-53.,-40.,-37.,-28.,-23.,-22.,27*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c	data bf/2.6,2.4,2.3,2.1,2.,1.9,1.8,
c     *2.6,2.4,2.4,2.1,2.1,1.9,27*0./

cccccccccc///////!ATENTION! NEXT IS S36!!!////////////////cccccccccccccc

cccccccccccc S36 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/12/,l/5,7,0/,alo/2,4,2,6,2,2,4,2,6,2,4,0,7*0/
c      data al/0.,1.,1.,2.,0.,0.,1.,1.,2.,0.,2.,3.,7*0./
c      data aj/.5,1.5,.5,2.5,0.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,7*0./
c      data en/-28.,-17.,-14.,-10.,-7.,
c     *-36.,-26.,-22.,-14.,-9.,-6.,-1.,7*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.78,
c     *1.65,1.67,1.7,1.7,1.75,1.77,1.8,7*0./

cccccccccccc Cl37 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/13/,l/6,7,0/,alo/2,4,2,6,2,1,2,4,2,6,2,4,0,6*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,3.,6*0./
c      data aj/.5,1.5,.5,2.5,0.5,1.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,6*0./
c      data en/-43.,-30.,-25.,-16.,-10.,-7.,
c     *-46.,-31.,-27.,-18.,-13.,-10.,-5.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.78,1.8,
c     *1.65,1.67,1.7,1.7,1.75,1.77,1.8,6*0./

cccccccccccc Ar38 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/13/,l/6,7,0/,alo/2,4,2,6,2,2,2,4,2,6,2,4,0,6*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,3.,6*0./
c      data aj/.5,1.5,.5,2.5,0.5,1.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,6*0./
c      data en/-43.,-30.,-25.,-16.,-10.,-7.,
c     *-46.,-31.,-27.,-18.,-13.,-10.,-5.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.78,1.8,
c     *1.65,1.67,1.7,1.7,1.75,1.77,1.8,6*0./

cccccccccccc K39 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/13/,l/6,7,0/,alo/2,4,2,6,2,3,2,4,2,6,2,4,0,6*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,3.,6*0./
c      data aj/.5,1.5,.5,2.5,0.5,1.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,6*0./
c      data en/-43.,-30.,-25.,-16.,-10.,-7.,
c     *-46.,-31.,-27.,-18.,-13.,-10.,-5.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.78,1.8,
c     *1.65,1.67,1.7,1.7,1.75,1.77,1.8,6*0./

cccccccccccc Ca40 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/13/,l/6,7,0/,alo/2,4,2,6,2,4,2,4,2,6,2,4,0,27*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,3.,27*0./
c      data aj/.5,1.5,.5,2.5,0.5,1.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,27*0./
c      data en/-43.,-30.,-25.,-16.,-10.,-7.,
c     *-46.,-31.,-27.,-18.,-13.,-10.,-5.,27*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.78,1.8,
c     *1.65,1.67,1.7,1.7,1.75,1.77,1.8,27*0./

cccccccccccc Sc41 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,2,4,1,2,4,2,6,2,4,0,5*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2.,0.,2.,3.,5*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,.5,1.5,.5,2.5,.5,1.5,3.5,5*0./
c      data en/-40.,-27.,-23.,-16.,-10.,-7.,-5.,
c     *-46.,-31.,-27.,-18.,-13.,-10.,-5.,5*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.78,1.8,1.82,
c     *1.65,1.67,1.7,1.7,1.75,1.77,1.8,5*0./

cccccccccccc Ti42 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,2,4,2,2,4,2,6,2,4,0,5*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2.,0.,2.,3.,5*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,.5,1.5,.5,2.5,.5,1.5,3.5,5*0./
c      data en/-40.,-27.,-23.,-16.,-10.,-7.,-5.,
c     *-46.,-31.,-27.,-18.,-13.,-10.,-5.,5*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.78,1.8,1.82,
c     *1.65,1.67,1.7,1.7,1.75,1.77,1.8,5*0./

cccccccccccc V43(orig)&& cccccccccccccccccccccccccccccccccccccccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,2,4,3,2,4,2,6,2,4,0,5*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2.,0.,2.,3.,5*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,.5,1.5,.5,2.5,.5,1.5,3.5,5*0./
c      data en/-40.,-27.,-23.,-16.,-10.,-7.,-5.,
c     *-46.,-31.,-27.,-18.,-13.,-10.,-5.,5*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.78,1.8,1.82,
c     *1.65,1.67,1.7,1.7,1.75,1.77,1.8,5*0./
cccccccccccc V43(SkIII)&& cccccccccccccccccccccccccccccccccccccccc
c      Data ln/13/,l/7,6,0/,alo/2,4,2,6,2,4,3,2,4,2,6,2,4,0,5*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2.,0.,2.,3.,5*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,.5,1.5,.5,2.5,.5,1.5,3.5,5*0./
c      data en/-40.,-27.,-23.,-16.,-10.,-7.,-5.,
c     *-46.,-31.,-27.,-18.,-13.,-10.,-5.,5*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.78,1.8,1.82,
c     *1.65,1.67,1.7,1.7,1.75,1.77,1.8,5*0./
cccccccccccc Cr44 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,2,4,4,2,4,2,6,2,4,0,5*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2.,0.,2.,3.,5*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,.5,1.5,.5,2.5,.5,1.5,3.5,5*0./
c      data en/-40.,-27.,-23.,-16.,-10.,-7.,-5.,
c     *-46.,-31.,-27.,-18.,-13.,-10.,-5.,5*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.78,1.8,1.82,
c     *1.65,1.67,1.7,1.7,1.75,1.77,1.8,5*0./

cccccccccccc Mn45 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,2,4,5,2,4,2,6,2,4,0,5*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2.,0.,2.,3.,5*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,.5,1.5,.5,2.5,.5,1.5,3.5,5*0./
c      data en/-40.,-27.,-23.,-13.,-9.,-7.,-1.,
c     *-50.,-38.,-34.,-25.,-20.,-19.,-13.,5*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.78,1.8,1.82,
c     *1.65,1.67,1.7,1.7,1.75,1.77,1.8,5*0./

cccccccccccc Fe46 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,2,4,6,2,4,2,6,2,4,0,5*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2.,0.,2.,3.,5*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,.5,1.5,.5,2.5,.5,1.5,3.5,5*0./
c      data en/-40.,-27.,-23.,-13.,-9.,-7.,-1.,
c     *-50.,-38.,-34.,-25.,-20.,-19.,-13.,5*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.78,1.8,1.82,
c     *1.65,1.67,1.7,1.7,1.75,1.77,1.8,5*0./

cccccccccccc Co47 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,2,4,7,2,4,2,6,2,4,0,5*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2.,0.,2.,3.,5*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,.5,1.5,.5,2.5,.5,1.5,3.5,5*0./
c      data en/-40.,-27.,-23.,-13.,-9.,-7.,-1.,
c     *-50.,-38.,-34.,-25.,-20.,-19.,-13.,5*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.78,1.8,1.82,
c     *1.65,1.67,1.7,1.7,1.75,1.77,1.8,5*0./

cccccccccccc Ni48 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,2,4,8,2,4,2,6,2,4,0,5*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2.,0.,2.,3.,5*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,.5,1.5,.5,2.5,.5,1.5,3.5,5*0./
c      data en/-40.,-27.,-23.,-13.,-9.,-7.,-1.,
c     *-50.,-38.,-34.,-25.,-20.,-19.,-13.,5*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.78,1.8,1.82,
c     *1.65,1.67,1.7,1.7,1.75,1.77,1.8,5*0./



cccccc/////// END of N = 20 /////////////////////////////////////


cccccccccccc O28&& cccccccccccccccccccccccccccccccccccccccc
c      Data ln/9/,l/3,6,0/,alo/2,4,2,2,4,2,6,2,4,0,9*0/
c      data al/0.,1.,1.,0.,1.,1.,2.,0.,2.,0.,9*0./
c      data aj/.5,1.5,.5,0.5,1.5,0.5,2.5,0.5,1.5,10*0./
c      data en/-36.,-22.,-14.,
c     *-36.,-22.,-19.,-14.,-10.,-5.,10*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.65,1.67,1.7,1.7,1.74,1.78,10*0./

cccccccccccc Si28 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/8/,l/4,4,0/,alo/2,4,2,5,2,4,2,6,0,0,0,8*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,0.,0.,8*0./
c      data aj/.5,1.5,.5,2.5,0.5,1.5,0.5,2.5,11*0./
c      data en/-36.,-22.,-14.,-7.,
c     *-36.,-22.,-19.,-14.,11*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.65,1.67,1.7,1.7,11*0./


cccccc///////////// N = 28 ///////////////////////////////////////////

cccccccccccc Mg40& (doesnt work for SkIII) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/11/,l/4,7,0/,alo/2,4,2,4,2,4,2,6,2,4,8,0,7*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,2.,3.,0.,7*0./
c      data aj/.5,1.5,.5,2.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,0.,7*0./
c0      data en/-50.,-38.,-35.,-26.,
c0     *-40.,-27.,-25.,-15.,-10.,-9.,-3.,0.,7*0./
c1      data en/-45.,-36.,-33.,-25.,
c1     *-31.,-21.,-19.,-12.,-10.,-7.,-3.,0.,7*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c0      data bf/1.65,1.7,1.74,1.77,
c0     *1.65,1.67,1.7,1.72,1.74,1.8,1.82,0.,7*0./
c c1    for Skp:
c1      data bf/1.64,1.72,1.72,1.76,
c1     *1.69,1.72,1.76,1.78,1.84,1.86,1.86,8*0./

cccccccccccc Al41 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/11/,l/4,7,0/,alo/2,4,2,5,2,4,2,6,2,4,8,0,7*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,2.,3.,0.,7*0./
c      data aj/.5,1.5,.5,2.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,0.,7*0./
c0      data en/-36.,-22.,-14.,-7.,
c0     *-36.,-22.,-19.,-14.,-10.,-7.,-3.,0.,7*0./
c c1    for Skp:
c1      data en/-45.,-34.,-32.,-24.,
c1     *-32.,-22.,-20.,-13.,-11.,-8.,-4.,0.,7*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.64,1.72,1.72,1.76,
c     *1.67,1.69,1.76,1.76,1.82,1.84,1.86,8*0./

cccccccccccc Si42 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/11/,l/4,7,0/,alo/2,4,2,6,2,4,2,6,2,4,8,0,7*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,0.,2.,3.,0.,7*0./
c      data aj/.5,1.5,.5,2.5,0.5,1.5,0.5,2.5,0.5,1.5,3.5,0.,7*0./
c      data en/-36.,-22.,-14.,-7.,
c     *-36.,-22.,-19.,-14.,-10.,-7.,-3.,0.,7*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.65,1.67,1.7,1.7,1.7,1.8,1.8,0.,7*0./

cccccccccccc P43 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/12/,l/5,7,0/,alo/2,4,2,6,1,2,4,2,6,2,4,8,0,6*0/
c      data al/0.,1.,1.,2.,0.,0.,1.,1.,2.,0.,2.,3.,0.,6*0./
c      data aj/.5,1.5,.5,2.5,.5,.5,1.5,0.5,2.5,0.5,1.5,3.5,0.,6*0./
c      data en/-51.,-38.,-35.,-25.,-17.,
c     *-40.,-31.,-28.,-17.,-13.,-10.,-3.,0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.78,
c     *1.65,1.67,1.7,1.7,1.7,1.82,1.84,0.,6*0./

cccccccccccc S44 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/12/,l/5,7,0/,alo/2,4,2,6,2,2,4,2,6,2,4,8,0,6*0/
c      data al/0.,1.,1.,2.,0.,0.,1.,1.,2.,0.,2.,3.,0.,6*0./
c      data aj/.5,1.5,.5,2.5,.5,.5,1.5,0.5,2.5,0.5,1.5,3.5,0.,6*0./
c      data en/-51.,-38.,-35.,-25.,-17.,
c     *-46.,-31.,-28.,-17.,-13.,-10.,-3.,0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.78,
c     *1.65,1.67,1.7,1.7,1.7,1.8,1.8,0.,6*0./

cccccccccccc Cl45 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/13/,l/6,7,0/,alo/2,4,2,6,2,1,
c     *2,4,2,6,2,4,8,6*0/
c      data al/0.,1.,1.,2.,0.,2.,
c     *0.,1.,1.,2.,0.,2.,3.,6*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,
c     *.5,1.5,0.5,2.5,0.5,1.5,3.5,6*0./
c      data en/-47.,-38.,-35.,-25.,-17.,-10.,
c     *-42.,-31.,-28.,-17.,-13.,-10.,-3.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.78,1.8,
c     *1.65,1.67,1.7,1.7,1.7,1.8,1.8,6*0./

cccccccccccc Ar46 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/13/,l/6,7,0/,alo/2,4,2,6,2,2,
c     *2,4,2,6,2,4,8,6*0/
c      data al/0.,1.,1.,2.,0.,2.,
c     *0.,1.,1.,2.,0.,2.,3.,6*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,
c     *.5,1.5,0.5,2.5,0.5,1.5,3.5,6*0./
c      data en/-51.,-38.,-35.,-25.,-17.,-10.,
c     *-46.,-31.,-28.,-17.,-13.,-10.,-3.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.78,1.8,
c     *1.65,1.67,1.7,1.7,1.7,1.8,1.8,6*0./

cccccccccccc K47 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/13/,l/6,7,0/,alo/2,4,2,6,2,3,
c     *2,4,2,6,2,4,8,6*0/
c      data al/0.,1.,1.,2.,0.,2.,
c     *0.,1.,1.,2.,0.,2.,3.,6*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,
c     *.5,1.5,0.5,2.5,0.5,1.5,3.5,6*0./
c      data en/-51.,-38.,-35.,-25.,-17.,-10.,
c     *-46.,-31.,-28.,-17.,-13.,-10.,-3.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.78,1.8,
c     *1.65,1.67,1.7,1.7,1.7,1.8,1.8,6*0./

cccccccccccc Ca48 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/13/,l/6,7,0/,alo/2,4,2,6,2,4,
c     *2,4,2,6,2,4,8,27*0/
c      data al/0.,1.,1.,2.,0.,2.,
c     *0.,1.,1.,2.,0.,2.,3.,27*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,
c     *.5,1.5,0.5,2.5,0.5,1.5,3.5,27*0./
c      data en/-51.,-38.,-35.,-25.,-17.,-10.,
c     *-46.,-31.,-28.,-17.,-13.,-10.,-3.,27*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/2.6,2.4,2.4,2.1,2.1,1.9,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,27*0./

cccccccccccccc Ca48 from izotopes cccccccccccccccccccc
c      data ln/13/,l/6,7,0/,
c     *alo/2,4,2,6,2,4,2,4,2,6,2,4,8,6*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,3.,6*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,6*0./
c      data en/-41.8,-27.4,-23.2,-14.3,-10.4,-8.9,
c     *-47.9,-34.5,-31.1,-22.3,-17.1,-15.9,-10.0,6*0./
c      data bf/2.6,2.4,2.4,2.1,2.1,1.9,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,6*0./

cccccccccccc Sc49 (c2 for SkIII) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,2,4,1,2,4,2,6,2,4,8,5*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,
c     *0.,1.,1.,2.,0.,2.,3.,5*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,
c     *.5,1.5,0.5,2.5,0.5,1.5,3.5,5*0./
c      data en/-43.,-32.,-30.,-21.,-15.,-16.,-10.,
c     *-46.,-31.,-28.,-17.,-13.,-10.,-3.,5*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c0      data bf/1.7,1.74,1.8,1.9,2.1,2.4,
c0     *1.65,1.67,1.7,1.7,1.7,1.8,1.8,6*0./
c1      data bf/1.62,1.7,1.7,1.74,1.78,1.8,1.9,
c1     *1.65,1.67,1.7,1.7,1.7,1.8,1.8,5*0./
c2      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.8,
c2     *2.6,2.4,2.3,2.1,2.,1.9,1.8,5*0./

cccccccccccc Ti50 (c2 for SkIII) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,2,4,2,2,4,2,6,2,4,8,5*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,
c     *0.,1.,1.,2.,0.,2.,3.,5*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,
c     *.5,1.5,0.5,2.5,0.5,1.5,3.5,5*0./
c      data en/-51.,-38.,-35.,-25.,-17.,-16.,-10.,
c     *-46.,-31.,-28.,-17.,-13.,-10.,-3.,5*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c0      data bf/1.62,1.7,1.7,1.74,1.78,1.8,1.82,
c0     *1.65,1.67,1.7,1.7,1.7,1.8,1.8,5*0./
c1      data bf/1.62,1.7,1.7,1.74,1.78,1.8,1.9,
c1     *1.65,1.67,1.7,1.7,1.7,1.8,1.8,5*0./
c2      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.8,
c2     *2.6,2.4,2.3,2.1,2.,1.9,1.8,5*0./

cccccccccccc V51 (c2 for SkIII) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,2,4,3,2,4,2,6,2,4,8,5*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,
c     *0.,1.,1.,2.,0.,2.,3.,5*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,
c     *.5,1.5,0.5,2.5,0.5,1.5,3.5,5*0./
c      data en/-51.,-38.,-35.,-25.,-17.,-16.,-10.,
c     *-46.,-31.,-28.,-17.,-13.,-10.,-3.,5*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c0      data bf/1.62,1.7,1.7,1.74,1.78,1.8,1.82,
c0     *1.65,1.67,1.7,1.7,1.7,1.8,1.8,5*0./
c1      data bf/1.62,1.7,1.7,1.74,1.78,1.8,1.9,
c1     *1.65,1.67,1.7,1.7,1.7,1.8,1.8,5*0./
c2      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.8,
c2     *2.6,2.4,2.3,2.1,2.,1.9,1.8,5*0./

cccccccccccc Cr52 (c2 for SkIII c3 for Skp) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,2,4,4,2,4,2,6,2,4,8,5*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,
c     *0.,1.,1.,2.,0.,2.,3.,5*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,
c     *.5,1.5,0.5,2.5,0.5,1.5,3.5,5*0./
c0      data en/-51.,-38.,-35.,-25.,-17.,-16.,-10.,
c0     *-46.,-31.,-28.,-17.,-13.,-10.,-3.,5*0./
c3      data en/-35.,-27.,-25.,-18.,-15.,-13.,-9.,
c3     *-39.,-30.,-28.,-21.,-18.,-16.,-12.,5*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c0      data bf/1.62,1.7,1.7,1.74,1.78,1.8,1.82,
c0     *1.65,1.67,1.7,1.7,1.7,1.8,1.8,5*0./
c1      data bf/1.62,1.7,1.7,1.74,1.78,1.8,1.9,
c1     *1.65,1.67,1.7,1.7,1.7,1.8,1.8,5*0./
c2      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.8,
c2     *2.6,2.4,2.3,2.1,2.,1.9,1.8,5*0./

cccccccccccc Mn53 (c2 for SkIII) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,2,4,5,2,4,2,6,2,4,8,5*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,
c     *0.,1.,1.,2.,0.,2.,3.,5*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,
c     *.5,1.5,0.5,2.5,0.5,1.5,3.5,5*0./
c0      data en/-51.,-38.,-35.,-25.,-17.,-16.,-10.,
c0     *-46.,-31.,-28.,-17.,-13.,-10.,-3.,5*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c0      data bf/1.62,1.7,1.7,1.74,1.78,1.8,1.82,
c0     *1.65,1.67,1.7,1.7,1.7,1.8,1.8,5*0./
c1      data en/-35.,-26.,-24.,-17.,-14.,-12.,-8.,
c1     *-39.,-30.,-28.,-21.,-18.,-17.,-12.,5*0./
c3      data bf/1.62,1.7,1.7,1.74,1.78,1.8,1.9,
c3     *1.65,1.67,1.7,1.7,1.7,1.8,1.8,5*0./
c2      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.8,
c2     *2.6,2.4,2.3,2.1,2.,1.9,1.8,5*0./

cccccccccccc Fe54 (c2 for SkIII) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,2,4,6,2,4,2,6,2,4,8,5*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,
c     *0.,1.,1.,2.,0.,2.,3.,5*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,
c     *.5,1.5,0.5,2.5,0.5,1.5,3.5,5*0./
c0      data en/-40.,-30.,-27.,-19.,-13.,-13.,-8.,
c0     *-46.,-36.,-33.,-25.,-20.,-19.,-14.,5*0./
c3      data en/-34.,-26.,-24.,-17.,-16.,-12.,-8.,
c3     *-40.,-31.,-29.,-22.,-19.,-18.,-13.,5*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c0      data bf/1.62,1.7,1.7,1.74,1.78,1.8,1.82,
c0     *1.65,1.67,1.7,1.7,1.7,1.8,1.8,5*0./
c1      data bf/1.62,1.7,1.7,1.74,1.78,1.8,1.82,
c1     *1.65,1.67,1.7,1.7,1.7,1.8,1.8,5*0./
c2      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.8,
c2     *2.6,2.4,2.3,2.1,2.,1.9,1.8,5*0./

cccccccccccc Co55 (c2 for SkIII) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/14/,l/7,7,0/,alo/2,4,2,6,2,4,7,2,4,2,6,2,4,8,5*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,
c     *0.,1.,1.,2.,0.,2.,3.,5*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,
c     *.5,1.5,0.5,2.5,0.5,1.5,3.5,5*0./
c0      data en/-40.,-30.,-27.,-19.,-13.,-13.,-8.,
c0     *-46.,-36.,-33.,-25.,-20.,-19.,-14.,5*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c3      data en/-36.,-25.,-23.,-16.,-13.,-12.,-7.,
c3     *-40.,-32.,-30.,-23.,-20.,-19.,-14.,5*0./
c0      data bf/1.62,1.7,1.7,1.74,1.78,1.8,1.82,
c0     *1.65,1.67,1.7,1.7,1.7,1.8,1.8,5*0./
c1      data bf/1.62,1.7,1.7,1.74,1.78,1.8,1.9,
c1     *1.65,1.67,1.7,1.7,1.7,1.8,1.8,5*0./
c2      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.8,
c2     *2.6,2.4,2.3,2.1,2.,1.9,1.8,5*0./

cccccccccccc Cu57& (c2 for SkIII) (doesnt work for SkM*) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/15/,l/8,7,0/,alo/2,4,2,6,2,4,8,1,2,4,2,6,2,4,8,4*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,
c     *0.,1.,1.,2.,0.,2.,3.,4*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,1.5,
c     *.5,1.5,0.5,2.5,0.5,1.5,3.5,4*0./
c      data en/-43.,-31.,-29.,-19.,-13.,-13.,-10.,-7.,
c     *-46.,-31.,-28.,-17.,-13.,-10.,-3.,4*0./
c3      data en/-32.,-24.,-22.,-15.,-12.,-11.,-6.,-3.,
c3     *-42.,-33.,-31.,-24.,-22.,-20.,-15.,4*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c0      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.7,1.6,
c0     *2.6,2.4,2.3,2.1,2.,1.9,1.8,4*0./
c1      data bf/1.62,1.7,1.7,1.74,1.78,1.8,1.9,1.9,
c1     *1.65,1.67,1.7,1.7,1.7,1.8,1.8,4*0./
c2      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.8,1.7,
c2     *2.6,2.4,2.3,2.1,2.,1.9,1.8,4*0./

cccccccccccc Zn58 (c2 for SkIII) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/15/,l/8,7,0/,alo/2,4,2,6,2,4,8,2,2,4,2,6,2,4,8,4*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,
c     *0.,1.,1.,2.,0.,2.,3.,4*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,1.5,
c     *.5,1.5,0.5,2.5,0.5,1.5,3.5,4*0./
c0      data en/-43.,-31.,-29.,-19.,-13.,-13.,-10.,-7.,
c0     *-46.,-31.,-28.,-17.,-13.,-10.,-3.,4*0./
c3      data en/-32.,-23.,-21.,-15.,-12.,-10.,-6.,-3.,
c3     *-43.,-34.,-32.,-25.,-22.,-20.,-16.,4*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c1      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.7,1.6,
c1     *2.6,2.4,2.3,2.1,2.,1.9,1.8,4*0./
c2      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.8,1.7,
c2     *2.6,2.4,2.3,2.1,2.,1.9,1.8,4*0./

cccccccccccc Ga59 (c2 for SkIII) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/15/,l/8,7,0/,alo/2,4,2,6,2,4,8,3,2,4,2,6,2,4,8,4*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,
c     *0.,1.,1.,2.,0.,2.,3.,4*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,1.5,
c     *.5,1.5,0.5,2.5,0.5,1.5,3.5,4*0./
c0      data en/-43.,-31.,-29.,-19.,-13.,-13.,-10.,-7.,
c0     *-46.,-31.,-28.,-17.,-13.,-10.,-3.,4*0./
c3      data en/-31.,-23.,-21.,-14.,-11.,-10.,-6.,-2.,
c3     *-43.,-35.,-33.,-26.,-23.,-21.,-17.,4*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c0      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.7,1.6,
c0     *2.6,2.4,2.3,2.1,2.,1.9,1.8,4*0./
c2      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.8,1.7,
c2     *2.6,2.4,2.3,2.1,2.,1.9,1.8,4*0./

cccccccccccc Ge60 (c2 for SkIII) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/15/,l/8,7,0/,alo/2,4,2,6,2,4,8,4,2,4,2,6,2,4,8,4*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,
c     *0.,1.,1.,2.,0.,2.,3.,4*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,1.5,
c     *.5,1.5,0.5,2.5,0.5,1.5,3.5,4*0./
c0      data en/-43.,-31.,-29.,-19.,-13.,-13.,-10.,-7.,
c0     *-46.,-31.,-28.,-17.,-13.,-10.,-3.,4*0./
c3      data en/-30.,-22.,-20.,-14.,-11.,-10.,-5.,-1.,
c3     *-44.,-35.,-33.,-26.,-24.,-22.,-17.,4*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c0      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.7,1.6,
c0     *2.6,2.4,2.3,2.1,2.,1.9,1.8,4*0./
c2      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.8,1.7,
c2     *2.6,2.4,2.3,2.1,2.,1.9,1.8,4*0./

cccccccccccc As61&& (doesnt work, check input!) cccccccccccccccccccccccccccccccccccccccc
c      Data ln/16/,l/9,7,0/,alo/2,4,2,6,2,4,8,4,1,2,4,2,6,2,4,8,3*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,2.,
c      *0.,1.,1.,2.,0.,2.,3.,3*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,
c      *.5,1.5,0.5,2.5,0.5,1.5,3.5,3*0./
c      data en/-42.,-30.,-27.,-17.,-13.,-12.,-10.,-5,-3.,
c      *-56.,-43.,-40.,-30.,-26.,-25.,-18.,3*0./
c3      data en/-29.,-22.,-19.,-14.,-10.,-9.,-5.,-4,-3.,
c3     *-45.,-36.,-34.,-27.,-24.,-23.,-18.,3*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c0      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.7,1.6,1.6,
c0     *2.6,2.4,2.3,2.1,2.,1.9,1.8,3*0./
c2      data bf/2.64,2.44,2.44,2.14,2.14,1.94,1.92,1.4,1.3,
c2     *2.6,2.4,2.3,2.1,2.,1.9,1.8,3*0./
c3      data bf/2.62,2.42,2.42,2.12,2.12,1.92,1.72,1.62,1.62,
c3     *2.6,2.4,2.3,2.1,2.,1.9,1.8,3*0./

cccccc///////////// END of N = 28 ///////////////////////////////////////////


cccccccccccc S32 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/13/,l/5,8,0/,alo/2,4,2,6,2,2,4,2,6,2,4,8,2,6*0/
c      data al/0.,1.,1.,2.,0.,0.,1.,1.,2.,0.,2.,3.,1.,6*0./
c      data aj/.5,1.5,.5,2.5,.5,.5,1.5,.5,2.5,.5,1.5,3.5,1.5,6*0./
c      data en/-30.,-20.,-15.,-9.,-5.,
c     *-41.,-29.,-24.,-18.,-14.,-10.,-8.,-3.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.78,
c     *1.60,1.64,1.68,1.73,1.76,1.8,1.8,1.86,6*0./

cccccccccccc Mg23 cccccccccccccccccccccccccccccccccccccccc
c      Data ln/8/,l/4,4,0/,alo/2,4,2,4,2,4,2,4,5*0,6*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,5*0.,6*0./
c      data aj/.5,1.5,.5,2.5,0.5,1.5,0.5,2.5,5*0.,6*0./
c      data en/-36.5,-21.9,-14.2,-7.,-36.5,-21.9,-14.2,-7.,5*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.62,1.7,1.7,1.74,11*0./

cccccccccccc Mg24L cccccccccccccccccccccccccccccccccccccccc
C      Data ln/9/,l/4,4,1/,alo/2,4,2,4,2,4,2,3,1,10*0/
C      data al/0.,1.,1.,2.,0.,1.,1.,2.,1.,10*0./
C      data aj/.5,1.5,.5,2.5,0.5,1.5,0.5,2.5,1.5,10*0./
C      data en/-36.5,-21.9,-14.2,-7.,-36.5,-21.9,-14.2,-7.,-10.,10*0./
C      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
C      data bf/1.62,1.7,1.7,1.74,1.62,1.7,1.7,1.74,1.7,10*0./

cccccccccccc Ne24L cccccccccccccccccccccccccccccccccccccccc
c      Data ln/9/,l/4,4,1/,alo/2,4,2,2,2,4,2,5,1,10*0/
c      data al/0.,1.,1.,2.,0.,1.,1.,2.,2.,10*0./
c      data aj/.5,1.5,.5,2.5,0.5,1.5,0.5,2.5,2.5,10*0./
c      data en/-39.5,-25.9,-20.2,-11.,-40.5,-23.9,-19.2,-10.,-3.,10*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.7,1.74,1.62,1.7,1.7,1.74,1.8,10*0./
c	data bf/2.6,2.4,2.4,2.1,
c     *2.6,2.4,2.3,2.1,2.,1.9,0.,8*0./

cccccccccccc c13L cccccccccccccccccccccccccccccccccccccccc
c      Data ln/5/,l/2,2,1/,alo/2,4,2,4,1,0,7*0,6*0/
c      data al/0.,1.,0.,1.,1.,0.,7*0.,6*0./
c      data aj/.5,1.5,.5,1.5,1.5,0.,0.,6*0.,6*0./
c      data en/-26.5,-11.9,-34.2,-8.,-2.,0.,7*0.,6*0./
c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
c      data bf/1.62,1.7,1.6,1.74,1.6,0.,0.,12*0./

ccccccccccccccccc  C14  cccccccccccccccccccccccc
C      Data ln/7/,l/2,4,1/,alo/2,4,2,4,2,1,1,6*0,6*0/
C      data al/0.,1.,0.,1.,1.,2.,0.,6*0.,6*0./
C      data aj/.5,1.5,.5,1.5,0.5,2.5,0.5,6*0.,6*0./
C      data en/-32.,-20.,-30.,-16.,-10.,-1.,-10.,6*0.,6*0./
C      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/
C      data bf/1.62,1.7,1.6,1.74,1.7,1.8,2.2,12*0./

cccccccccccccc Fe56 cccccccccccccccccccc
c		data ln/15/,l/7,8,0/,
c     *alo/2,4,2,6,2,4,6,2,4,2,6,2,4,8,2,0,3*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2.,0.,2.,3.,1.,0.,3*0./
c		data aj/.5,1.5,.5,2.5,.5,1.5,3.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,1.5,0.,3*0./
c      data en/-41.8,-31.4,-29.2,-20.3,-15.4,-14.9,-9.1,
c     *-47.9,-37.5,-35.1,-26.3,-21.1,-20.9,-14.9,-10.,0.,3*0./
c      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.7,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,1.7,0.,3*0./

cccccc///////// Calcium isotopes (doesnt work for SkIII?) /////////////////////////////////////
cccccccccccccc Ca34 cccccccccccccccccccc
c		data ln/10/,l/6,4,0/,
c     *alo/2,4,2,6,2,4,2,4,2,6,9*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,9*0./
c		data aj/.5,1.5,.5,2.5,.5,1.5,
c     *.5,1.5,.5,2.5,9*0./
c      data en/-27.8,-18.4,-15.2,-9.3,-6.4,-4.9,
c     *-38.9,-30.5,-27.1,-19.3,9*0./
c	data bf/2.6,2.4,2.4,2.1,1.9,1.8,
c     *2.6,2.4,2.3,2.1,9*0./

cccccccccccccc Ca35 cccccccccccccccccccc
c       data ln/11/,l/6,5,0/,
c     *alo/2,4,2,6,2,4,2,4,2,6,1,0,0,6*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,0.,0.,0.,0.,4*0./
c		data aj/.5,1.5,.5,2.5,.5,1.5,
c     *.5,1.5,.5,2.5,.5,0.,0.,6*0./
c      data en/-38.8,-24.4,-20.2,-11.3,-7.4,-4.9,
c     *-49.9,-35.5,-31.1,-22.3,-17.1,0.,0.,0.,5*0./
c	data bf/2.6,2.4,2.4,2.1,2.1,1.9,
c     *2.6,2.4,2.3,2.1,2.,0.,0.,6*0./

cccccccccccccc Ca36 cccccccccccccccccccc
c       data ln/11/,l/6,5,0/,
c     *alo/2,4,2,6,2,4,2,4,2,6,2,0,0,6*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,0.,0.,0.,0.,4*0./
c		data aj/.5,1.5,.5,2.5,.5,1.5,
c     *.5,1.5,.5,2.5,.5,0.,0.,6*0./
c      data en/-38.8,-24.4,-20.2,-11.3,-7.4,-4.9,
c     *-49.9,-35.5,-31.1,-22.3,-17.1,0.,0.,0.,5*0./
c	data bf/2.6,2.4,2.4,2.1,2.1,1.9,
c     *2.6,2.4,2.3,2.1,2.,0.,0.,6*0./

cccccccccccccc Ca37 cccccccccccccccccccc
c       data ln/12/,l/6,6,0/,
c     *alo/2,4,2,6,2,4,2,4,2,6,2,1,0,6*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,0.,0.,0.,4*0./
c		data aj/.5,1.5,.5,2.5,.5,1.5,
c     *.5,1.5,.5,2.5,.5,1.5,0.,6*0./
c      data en/-41.8,-31.4,-29.2,-20.3,-15.4,-14.9,
c     *-47.9,-37.5,-35.1,-26.3,-21.1,-20.9,0.,0.,5*0./
c	data bf/2.6,2.4,2.4,2.1,2.1,1.9,
c     *2.6,2.4,2.3,2.1,2.,1.9,0.,6*0./

cccccccccccccc Ca38 cccccccccccccccccccc
c       data ln/12/,l/6,6,0/,
c     *alo/2,4,2,6,2,4,2,4,2,6,2,2,0,6*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,0.,0.,0.,4*0./
c		data aj/.5,1.5,.5,2.5,.5,1.5,
c     *.5,1.5,.5,2.5,.5,1.5,0.,6*0./
c      data en/-41.8,-31.4,-29.2,-20.3,-15.4,-14.9,
c     *-47.9,-37.5,-35.1,-26.3,-21.1,-20.9,0.,0.,5*0./
c	data bf/2.6,2.4,2.4,2.1,2.1,1.9,
c     *2.6,2.4,2.3,2.1,2.,1.9,0.,6*0./

cccccccccccccc Ca39 cccccccccccccccccccc
c       data ln/12/,l/6,6,0/,
c     *alo/2,4,2,6,2,4,2,4,2,6,2,3,0,6*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,0.,0.,0.,4*0./
c		data aj/.5,1.5,.5,2.5,.5,1.5,
c     *.5,1.5,.5,2.5,.5,1.5,0.,6*0./
c      data en/-41.8,-31.4,-29.2,-20.3,-15.4,-14.9,
c     *-47.9,-37.5,-35.1,-26.3,-21.1,-20.9,0.,0.,5*0./
c	data bf/2.6,2.4,2.4,2.1,2.1,1.9,
c     *2.6,2.4,2.3,2.1,2.,1.9,0.,6*0./

cccccccccccccc Ca40 cccccccccccccc0cccccc
c		data ln/12/,l/6,6,0/,
c     *alo/2,4,2,6,2,4,2,4,2,6,2,4,0,27*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,0.,27*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,
c     *.5,1.5,.5,2.5,.5,1.5,0.,27*0./
c      data en/-41.8,-31.4,-29.2,-20.3,-15.4,-14.9,
c     *-47.9,-37.5,-35.1,-26.3,-21.1,-20.9,0.,27*0./
c      data bf/2.6,2.4,2.4,2.1,2.1,1.9,
c     *2.6,2.4,2.3,2.1,2.,1.9,0.,27*0./

cccccccccccccc Ca40* cccccccccccccccccccc
c		data ln/14/,l/7,7,0/,
c     *alo/2,4,2,6,2,3,1,2,4,2,6,2,4,8,0,4*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,
c     *0.,1.,1.,2.,0.,2.,3.,0.,4*0./
c		data aj/.5,1.5,.5,2.5,.5,1.5,2.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,0.,4*0./
c      data en/-41.8,-31.4,-29.2,-20.3,-15.4,-14.9,-4.,
c     *-47.9,-37.5,-35.1,-26.3,-21.1,-14.9,-10.,0.,4*0./
c	    data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.7,
c     *2.6,2.4,2.3,2.1,2.,1.8,1.7,5*0./
cccccccccccccc Ca41 cccccccccccccccccccc
c		data ln/14/,l/7,7,0/,
c     *alo/2,4,2,6,2,4,4,2,4,2,6,2,4,1,26*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2.,0.,2.,3.,26*0./
c		data aj/.5,1.5,.5,2.5,.5,1.5,3.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,26*0./
c      data en/-41.8,-27.4,-23.2,-14.3,-10.4,-9.,-8.,
c     *-47.9,-34.5,-31.1,-22.3,-17.1,-15.9,-10.,26*0./
c	data bf/2.6,2.4,2.4,2.1,2.1,2.2,2.2,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.9,26*0./

cccccccccccccc Ca42 cccccccccccccccccccc
c               data ln/13/,l/6,7,0/,
c     *alo/2,4,2,6,2,4,2,4,2,6,2,4,2,6*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,3.,0.,0.,4*0./
c		data aj/.5,1.5,.5,2.5,.5,1.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,6*0./
c      data en/-41.8,-27.4,-23.2,-14.3,-10.4,-8.9,
c     *-47.9,-34.5,-31.1,-22.3,-17.1,-15.9,-10.0,0.,5*0./
c	data bf/2.6,2.4,2.4,2.1,2.1,1.9,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,6*0./

cccccccccccccc Ca43 cccccccccccccccccccc
c              data ln/13/,l/6,7,0/,
c     *alo/2,4,2,6,2,4,2,4,2,6,2,4,3,6*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,3.,0.,0.,4*0./
c		data aj/.5,1.5,.5,2.5,.5,1.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,6*0./
c      data en/-41.8,-27.4,-23.2,-14.3,-10.4,-8.9,
c     *-47.9,-34.5,-31.1,-22.3,-17.1,-15.9,-10.0,0.,5*0./
c	data bf/2.6,2.4,2.4,2.1,2.1,1.9,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,6*0./

cccccccccccccc Ca44 cccccccccccccccccccc
c              data ln/13/,l/6,7,0/,
c     *alo/2,4,2,6,2,4,2,4,2,6,2,4,4,6*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,3.,0.,0.,4*0./
c		data aj/.5,1.5,.5,2.5,.5,1.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,6*0./
c      data en/-41.8,-27.4,-23.2,-14.3,-10.4,-8.9,
c     *-47.9,-34.5,-31.1,-22.3,-17.1,-15.9,-10.0,0.,5*0./
c	data bf/2.6,2.4,2.4,2.1,2.1,1.9,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,6*0./

cccccccccccccc Ca45 cccccccccccccccccccc
c              data ln/13/,l/6,7,0/,
c     *alo/2,4,2,6,2,4,2,4,2,6,2,4,5,6*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,3.,0.,0.,4*0./
c		data aj/.5,1.5,.5,2.5,.5,1.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,6*0./
c      data en/-41.8,-27.4,-23.2,-14.3,-10.4,-8.9,
c     *-47.9,-34.5,-31.1,-22.3,-17.1,-15.9,-10.0,0.,5*0./
c	data bf/2.6,2.4,2.4,2.1,2.1,1.9,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,6*0./

cccccccccccccc Ca46 cccccccccccccccccccc
c              data ln/13/,l/6,7,0/,
c     *alo/2,4,2,6,2,4,2,4,2,6,2,4,6,6*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,3.,0.,0.,4*0./
c		data aj/.5,1.5,.5,2.5,.5,1.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,6*0./
c      data en/-41.8,-27.4,-23.2,-14.3,-10.4,-8.9,
c     *-47.9,-34.5,-31.1,-22.3,-17.1,-15.9,-10.0,0.,5*0./
c	data bf/2.6,2.4,2.4,2.1,2.1,1.9,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,6*0./

cccccccccccccc Ca47 cccccccccccccccccccc
c              data ln/13/,l/6,7,0/,
c     *alo/2,4,2,6,2,4,2,4,2,6,2,4,7,6*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,3.,0.,0.,4*0./
c		data aj/.5,1.5,.5,2.5,.5,1.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,6*0./
c      data en/-41.8,-27.4,-23.2,-14.3,-10.4,-8.9,
c     *-47.9,-34.5,-31.1,-22.3,-17.1,-15.9,-10.0,0.,5*0./
c	data bf/2.6,2.4,2.4,2.1,2.1,1.9,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,6*0./

cccccccccccccc Ca48 cccccccccccccccccccc
c              data ln/13/,l/6,7,0/,
c     *alo/2,4,2,6,2,4,2,4,2,6,2,4,8,6*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,3.,0.,0.,4*0./
c		data aj/.5,1.5,.5,2.5,.5,1.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,6*0./
c      data en/-41.8,-27.4,-23.2,-14.3,-10.4,-8.9,
c     *-47.9,-34.5,-31.1,-22.3,-17.1,-15.9,-10.0,0.,5*0./
c	data bf/2.6,2.4,2.4,2.1,2.1,1.9,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,6*0./

cccccccccccccc Ca49 cccccccccccccccccccc
c              data ln/14/,l/6,8,0/,
c     *alo/2,4,2,6,2,4,2,4,2,6,2,4,8,1,5*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,3.,1.,0.,4*0./
c		data aj/.5,1.5,.5,2.5,.5,1.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,1.5,5*0./
c      data en/-45.8,-34.4,-31.2,-22.3,-16.4,-16.0,
c     *-49.9,-36.5,-33.1,-22.3,-17.1,-15.9,-10.0,-5.0,5*0./
c	data bf/2.6,2.4,2.4,2.1,2.1,1.9,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,1.7,5*0./

cccccccccccccc Ca50 cccccccccccccccccccc
c		data ln/14/,l/6,8,0/,
c     *alo/2,4,2,6,2,4,2,4,2,6,2,4,8,2,5*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,3.,1.,0.,4*0./
c		data aj/.5,1.5,.5,2.5,.5,1.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,1.5,5*0./
c      data en/-39.8,-30.4,-28.2,-21.3,-18.4,-17.9,
c     *-34.9,-26.5,-23.1,-18.3,-15.1,-13.9,-10.,-6.,5*0./
c	    data bf/2.6,2.4,2.4,2.1,2.1,1.9,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,1.7,5*0./

cccccccccccccc Ca51 cccccccccccccccccccc
c		data ln/14/,l/6,8,0/,
c     *alo/2,4,2,6,2,4,2,4,2,6,2,4,8,3,5*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,3.,1.,0.,4*0./
c		data aj/.5,1.5,.5,2.5,.5,1.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,1.5,5*0./
c      data en/-39.8,-30.4,-28.2,-21.3,-18.4,-17.9,
c     *-34.9,-26.5,-23.1,-18.3,-15.1,-13.9,-10.,-6.,5*0./
c	    data bf/2.6,2.4,2.4,2.1,2.1,1.9,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,1.7,5*0./

cccccccccccccc Ca52 cccccccccccccccccccc
c      data ln/17/,l/7,10,0/,
c     *alo/2,4,2,6,2,4,0,2,4,2,6,2,4,8,4,0,0,23*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,23*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,23*0./
c      data en/-39.8,-30.4,-28.2,-21.3,-18.4,-17.9,-12.,
c     *-34.9,-26.5,-23.1,-18.3,-15.1,-13.9,-10.,-6.,-4.,-4.,23*0./
c      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.9,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,1.7,1.7,1.7,23*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/

cccccccccccccc Ca53 cccccccccccccccccccc
c		data ln/15/,l/6,9,0/,
c     *alo/2,4,2,6,2,4,2,4,2,6,2,4,8,4,1,4*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,3.,1.,3.,4*0./
c		data aj/.5,1.5,.5,2.5,.5,1.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,4*0./
c      data en/-39.8,-30.4,-28.2,-21.3,-18.4,-17.9,
c     *-34.9,-26.5,-23.1,-18.3,-15.1,-13.9,-10.,-6.,-3,4*0./
c	    data bf/2.6,2.4,2.4,2.1,2.1,1.9,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,1.7,1.6,4*0./

cccccccccccccc Ca54 cccccccccccccccccccc
c		data ln/15/,l/6,9,0/,
c     *alo/2,4,2,6,2,4,2,4,2,6,2,4,8,4,2,4*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,3.,1.,3.,4*0./
c		data aj/.5,1.5,.5,2.5,.5,1.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,4*0./
c      data en/-39.8,-30.4,-28.2,-21.3,-18.4,-17.9,
c     *-34.9,-26.5,-23.1,-18.3,-15.1,-13.9,-10.,-6.,-3,4*0./
c	    data bf/2.6,2.4,2.4,2.1,2.1,1.9,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,1.7,1.6,4*0./

cccccccccccccc Ca55 cccccccccccccccccccc
c		data ln/15/,l/6,9,0/,
c     *alo/2,4,2,6,2,4,2,4,2,6,2,4,8,4,3,4*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,3.,1.,3.,4*0./
c		data aj/.5,1.5,.5,2.5,.5,1.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,4*0./
c      data en/-39.8,-30.4,-28.2,-21.3,-18.4,-17.9,
c     *-34.9,-26.5,-23.1,-18.3,-15.1,-13.9,-10.,-6.,-3,4*0./
c	    data bf/2.6,2.4,2.4,2.1,2.1,1.9,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,1.7,1.6,4*0./

cccccccccccccc Ca56 cccccccccccccccccccc
c		data ln/15/,l/6,9,0/,
c     *alo/2,4,2,6,2,4,2,4,2,6,2,4,8,4,4,4*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,3.,1.,3.,4*0./
c		data aj/.5,1.5,.5,2.5,.5,1.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,4*0./
c      data en/-39.8,-30.4,-28.2,-21.3,-18.4,-17.9,
c     *-34.9,-26.5,-23.1,-18.3,-15.1,-13.9,-10.,-6.,-3,4*0./
c	    data bf/2.6,2.4,2.4,2.1,2.1,1.9,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,1.7,1.6,4*0./

cccccccccccccc Ca57 cccccccccccccccccccc
c		data ln/15/,l/6,9,0/,
c     *alo/2,4,2,6,2,4,2,4,2,6,2,4,8,4,5,4*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,3.,1.,3.,4*0./
c		data aj/.5,1.5,.5,2.5,.5,1.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,4*0./
c      data en/-39.8,-30.4,-28.2,-21.3,-18.4,-17.9,
c     *-34.9,-26.5,-23.1,-18.3,-15.1,-13.9,-10.,-6.,-3,4*0./
c	    data bf/2.6,2.4,2.4,2.1,2.1,1.9,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,1.7,1.6,4*0./

cccccccccccccc Ca58 cccccccccccccccccccc
c		data ln/15/,l/6,9,0/,
c     *alo/2,4,2,6,2,4,2,4,2,6,2,4,8,4,6,4*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,3.,1.,3.,4*0./
c		data aj/.5,1.5,.5,2.5,.5,1.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,4*0./
c      data en/-39.8,-30.4,-28.2,-21.3,-18.4,-17.9,
c     *-34.9,-26.5,-23.1,-18.3,-15.1,-13.9,-10.,-6.,-3,4*0./
c	    data bf/2.6,2.4,2.4,2.1,2.1,1.9,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,1.7,1.6,4*0./

ccccc////// END OF Ca IZOTOPES !!!/////////////////CCCCCCCCCC

cccccccccccccc Ti50 cccccccccccccccccccc
c		data ln/14/,l/7,7,0/,
c     *alo/2,4,2,6,2,4,2,2,4,2,6,2,4,8,0,0,3*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2.,0.,2.,3.,0.,0.,3*0./
c		data aj/.5,1.5,.5,2.5,.5,1.5,3.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,0.0,0.,3*0./
c      data en/-41.8,-31.4,-29.2,-20.3,-15.4,-14.9,-9.1,
c     *-47.9,-37.5,-35.1,-26.3,-21.1,-20.9,-14.9,0.,0.,3*0./
c      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.7,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,0.0,0.,3*0./


cccccccccccccc Ti48 cccccccccccccccccccc
c		data ln/14/,l/7,7,0/,
c     *alo/2,4,2,6,2,4,2,2,4,2,6,2,4,6,0,0,3*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2.,0.,2.,3.,0.,0.,3*0./
c		data aj/.5,1.5,.5,2.5,.5,1.5,3.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,0.0,0.,3*0./
c      data en/-33.8,-31.4,-29.2,-20.3,-15.4,-14.9,-9.1,
c     *-47.9,-37.5,-35.1,-26.3,-21.1,-20.9,-14.9,0.,0.,3*0./
c      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.7,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,0.0,0.,3*0./
cccccccccccccc Ti44 cccccccccccccccccccc
c		data ln/14/,l/7,7,0/,
c     *alo/2,4,2,6,2,4,2,2,4,2,6,2,4,2,5*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,0.,1.,1.,2.,0.,2.,3.,0.,4*0./
c		data aj/.5,1.5,.5,2.5,.5,1.5,3.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,5*0./
c      data en/-41.8,-31.4,-29.2,-20.3,-15.4,-14.9,-9.0,
c     *-47.9,-37.5,-35.1,-26.3,-21.1,-14.9,-9.,5*0./
c      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.7,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,0.,0.,3*0./
c
cccccccccccccc Ca44* cccccccccccccccccccc
c		data ln/14/,l/6,8,0/,
c     *alo/2,4,2,6,2,4,2,4,2,6,2,4,8,4,5*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,3.,1.,0.,4*0./
c		data aj/.5,1.5,.5,2.5,0.5,1.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,1.5,5*0./
c      data en/-41.8,-31.4,-29.2,-20.3,-15.4,-14.9,
c     *-47.9,-37.5,-35.1,-26.3,-21.1,-14.9,-8.,-6.,5*0./
c	    data bf/2.6,2.4,2.4,2.1,2.1,1.9,
c     *2.6,2.4,2.3,2.1,2.,1.8,1.7,1.5,5*0./
c      data h1,h2,n,ht,rt/.035,.34,200,.02,.455/
cccccccccccccc Ca52 cccccccccccccccccccc
c		data ln/14/,l/6,8,0/,
c     *alo/2,4,2,6,2,4,2,4,2,6,2,4,8,4,5*0/
c      data al/0.,1.,1.,2.,0.,2.,0.,1.,1.,2.,0.,2.,3.,1.,0.,4*0./
c		data aj/.5,1.5,.5,2.5,.5,1.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,1.5,5*0./
c      data en/-39.8,-30.4,-28.2,-21.3,-18.4,-17.9,
c     *-34.9,-26.5,-23.1,-18.3,-15.1,-13.9,-10.,-6.,5*0./
c	    data bf/2.6,2.4,2.4,2.1,2.1,1.9,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,1.7,5*0./
c      data h1,h2,n,ht,rt/.035,.34,200,.02,.455/

cccccccccccccc Ni56 (+4n +3p states) cccccccccccccccccccc
c      data ln/21/,l/10,11,0/,
c     *alo/2,4,2,6,2,4,8,0,0,0,2,4,2,6,2,4,8,0,0,0,0,0,18*0./
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,
c     *0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,4.,0.,18*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,4.5,0.,18*0./
c      data en/-45,-32.2,-30.6,-18.,-13.,-14.,-7.0,-2.,-0.5,-1.,
c     *-54.,-41.,-40.1,-27.3,-23.,-23.,-16.0,-12.0,-8.,-9.,-4.,19*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.8,1.8,1.8,1.8,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,1.7,1.6,1.5,1.4,19*0./
cccccccccccccc Ni56 (+4n +1p states) (�᫨ �뫥⠥� �� +4n +3p) ccccccc
c      data ln/19/,l/8,11,0/,
c     *alo/2,4,2,6,2,4,8,0,2,4,2,6,2,4,8,0,0,0,0,0,20*0./
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,
c     *0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,4.,0.,20*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,1.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,4.5,0.,20*0./
c      data en/-45,-32.2,-30.6,-18.,-13.,-14.,-7.0,-2.,
c     *-54.,-41.,-40.1,-27.3,-23.,-23.,-16.0,-12.0,-8.,-9.,-4.,21*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.8,1.8,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,1.7,1.6,1.5,1.4,21*0./

cccccccccccccc Ni58 (+3n +3p states) cccccccccccccccccccc
c      data ln/31/,l/15,16,0/,
c     *alo/2,4,2,6,2,4,8,0,0,0,0,0,0,0,0,
c     *2,4,2,6,2,4,8,2,0,0,0,0,0,0,0,0,9*0./
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,4.,2.,4.,0.,2.,
c     *0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,4.,2.,4.,0.,2.,5.,9*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,4.5,2.5,3.5,0.5,1.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,4.5,2.5,3.5,0.5,1.5,5.5,9*0./
c      data en/-45,-32.2,-30.6,-18.,-13.,-14.,-7.0,-6.,-6.,-3.,-3.,-1.,
c     *-1.,-0.05,-0.05,
c     *-54.,-41.,-40.1,-27.3,-23.,-23.,-16.0,-12.0,-8.,-9.,-4.,-2.,-2.,
c     *-1.,-0.05,-0.05,9*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.8,1.7,1.7,1.6,1.6,1.5,1.5,1.4,
c     *1.4,
c     *2.6,2.4,2.3,2.1,2.0,1.9,1.8,1.7,1.6,1.5,1.4,1.3,1.2,1.2,1.2,1.2,
c     *9*0./


cccccccccccccc Ni58 (+4n +3p states (Sitkov, 12.12.2024)) cccccccccccccccccccc
      data ln/22/,l/10,12,0/,
     *alo/2,4,2,6,2,4,8,0,0,0,
     *2,4,2,6,2,4,8,2,0,0,0,0,18*0./
      data al/0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,
     *0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,4.,2.,18*0./
      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,
     *.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,4.5,2.5,18*0./
      data en/-45,-32.2,-30.6,-18.,-13.,-14.,-7.0,-1.,-1.,-0.5,
     *-54.,-41.,-40.1,-27.3,-23.,-23.,-16.0,-12.0,-8.,-9.,-4.,-2.,
     *18*0./
      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.8,1.7,1.7,1.6,
     *2.6,2.4,2.3,2.1,2.0,1.9,1.8,1.7,1.6,1.5,1.4,1.3,
     *18*0./
cccccccccccccc Ni58 (+3n +2p states) (�᫨ �뫥⠥� �� +3n +3p)cccccccc
c      data ln/20/,l/9,11,0/,
c     *alo/2,4,2,6,2,4,8,0,0,2,4,2,6,2,4,8,2,0,0,0,0,0,18*0./
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,1.,
c     *0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,4.,0.,19*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,1.5,0.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,4.5,0.,19*0./
c      data en/-45,-32.2,-30.6,-18.,-13.,-14.,-7.0,-1.,-1.,
c     *-54.,-41.,-40.1,-27.3,-23.,-23.,-16.0,-12.0,-8.,-9.,-4.,20*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.8,1.8,1.8,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,1.7,1.6,1.5,1.4,20*0./
     
cccccccccccccc Ni60 (+3 neutron states) cccccccccccccccccccc
c      data ln/21/,l/10,11,0/,
c     *alo/2,4,2,6,2,4,8,0,0,0,2,4,2,6,2,4,8,4,0,0,0,0,18*0./
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,
c     *0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,4.,0.,18*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,4.5,0.,18*0./
c      data en/-45,-32.2,-30.6,-18.,-13.,-14.,-7.0,-1.,-1.,-1.,
c     *-54.,-41.,-40.1,-27.3,-23.,-23.,-16.0,-12.0,-8.,-9.,-4.,19*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.8,1.8,1.8,1.8,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,1.7,1.6,1.5,1.4,19*0./
     
cccccccccccccc Ni62 last 2p1/2 (+2 neutron states) cccccccccccccccccccc
c      data ln/21/,l/10,11,0/,
c     *alo/2,4,2,6,2,4,8,0,0,0,2,4,2,6,2,4,8,4,0,2,0,0,18*0./
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,
c     *0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,4.,0.,18*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,4.5,0.,18*0./
c      data en/-45,-32.2,-30.6,-18.,-13.,-14.,-7.0,-1.,-1.,-1.,
c     *-54.,-41.,-40.1,-27.3,-23.,-23.,-16.0,-12.0,-8.,-9.,-4.,19*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.8,1.8,1.8,1.8,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,1.7,1.6,1.5,1.4,19*0./

cccccccccccccc Ni64 2 on 2p1/2 and 2 on 1f5/2 (+1 neutron states) ccccc
c      data ln/21/,l/10,11,0/,
c     *alo/2,4,2,6,2,4,8,0,0,0,2,4,2,6,2,4,8,4,2,2,0,0,18*0./
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,
c     *0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,4.,0.,18*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,4.5,0.,18*0./
c      data en/-45,-32.2,-30.6,-18.,-13.,-14.,-7.0,-1.,-1.,-1.,
c     *-54.,-41.,-40.1,-27.3,-23.,-23.,-16.0,-12.0,-8.,-9.,-4.,19*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.8,1.8,1.8,1.8,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,1.7,1.6,1.5,1.4,19*0./
cccccccccccccc Ni64 2 on 2p1/2 and 2 on 1f5/2 (+1 neutron states) ccccc
c      data ln/20/,l/9,11,0/,
c     *alo/2,4,2,6,2,4,8,0,0,2,4,2,6,2,4,8,4,2,2,0,0,0,18*0./
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,3.,
c     *0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,4.,0.,19*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,4.5,0.,19*0./
c      data en/-45,-32.2,-30.6,-18.,-13.,-14.,-7.0,-1.,-1.,
c     *-54.,-41.,-40.1,-27.3,-23.,-23.,-16.0,-12.0,-8.,-9.,-4.,20*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.8,1.8,1.8,1.8,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,1.7,1.6,1.5,1.4,19*0./

cccccccccccccc Ni66 4 on 2p1/2 and 2 on 1f5/2 (+1 neutron states) ccccc
c      data ln/21/,l/10,11,0/,
c     *alo/2,4,2,6,2,4,8,0,0,0,2,4,2,6,2,4,8,4,4,2,0,0,18*0./
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,
c     *0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,4.,0.,18*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,4.5,0.,18*0./
c      data en/-45,-32.2,-30.6,-18.,-13.,-14.,-7.0,-1.,-1.,-1.,
c     *-54.,-41.,-40.1,-27.3,-23.,-23.,-16.0,-12.0,-8.,-9.,-4.,19*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.8,1.8,1.8,1.8,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,1.7,1.6,1.5,1.4,19*0./

cccccccccccccc Ni68 (+1 neutron states) ccccc
c      data ln/21/,l/10,11,0/,
c     *alo/2,4,2,6,2,4,8,0,0,0,2,4,2,6,2,4,8,4,6,2,0,0,18*0./
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,
c     *0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,4.,0.,18*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,4.5,0.,18*0./
c      data en/-45,-32.2,-30.6,-18.,-13.,-14.,-7.0,-1.,-1.,-1.,
c     *-54.,-41.,-40.1,-27.3,-23.,-23.,-16.0,-12.0,-8.,-9.,-4.,19*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.8,1.8,1.8,1.8,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,1.7,1.6,1.5,1.4,19*0./

cccccccccccccc Ni70 cccccccccccccccccccc
c      data ln/21/,l/10,11,0/,
c     *alo/2,4,2,6,2,4,8,0,0,0,2,4,2,6,2,4,8,4,6,2,2,0,18*0./
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,
c     *0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,4.,0.,18*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,4.5,0.,18*0./
c      data en/-45,-32.2,-30.6,-18.,-13.,-14.,-7.0,-1.,-1.,-1.,
c     *-54.,-41.,-40.1,-27.3,-23.,-23.,-16.0,-12.0,-8.,-9.,-4.,19*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.8,1.8,1.8,1.8,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,1.7,1.6,1.5,1.4,19*0./

cccccccccccccc Ni72 cccccccccccccccccccc
c      data ln/21/,l/10,11,0/,
c     *alo/2,4,2,6,2,4,8,0,0,0,2,4,2,6,2,4,8,4,6,2,4,0,18*0./
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,
c     *0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,4.,0.,18*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,4.5,0.,18*0./
c      data en/-45,-32.2,-30.6,-18.,-13.,-14.,-7.0,-1.,-1.,-1.,
c     *-54.,-41.,-40.1,-27.3,-23.,-23.,-16.0,-12.0,-8.,-9.,-4.,19*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.8,1.8,1.8,1.8,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,1.7,1.6,1.5,1.4,19*0./

cccccccccccccc Ni74 cccccccccccccccccccc
c      data ln/21/,l/10,11,0/,
c     *alo/2,4,2,6,2,4,8,0,0,0,2,4,2,6,2,4,8,4,6,2,6,0,18*0./
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,
c     *0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,4.,0.,18*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,4.5,0.,18*0./
c      data en/-45,-32.2,-30.6,-18.,-13.,-14.,-7.0,-1.,-1.,-1.,
c     *-54.,-41.,-40.1,-27.3,-23.,-23.,-16.0,-12.0,-8.,-9.,-4.,19*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.8,1.8,1.8,1.8,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,1.7,1.6,1.5,1.4,19*0./

cccccccccccccc Ni76 cccccccccccccccccccc
c      data ln/21/,l/10,11,0/,
c     *alo/2,4,2,6,2,4,8,0,0,0,2,4,2,6,2,4,8,4,6,2,8,0,18*0./
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,
c     *0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,4.,0.,18*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,4.5,0.,18*0./
c      data en/-45,-32.2,-30.6,-18.,-13.,-14.,-7.0,-1.,-1.,-1.,
c     *-54.,-41.,-40.1,-27.3,-23.,-23.,-16.0,-12.0,-8.,-9.,-4.,19*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/
c      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.8,1.8,1.8,1.8,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,1.7,1.6,1.5,1.4,19*0./

cccccccccccccc Ni78 cccccccccccccccccccc
c      data ln/21/,l/10,11,0/,
c     *alo/2,4,2,6,2,4,8,0,0,0,2,4,2,6,2,4,8,4,6,2,10,0,18*0./
c      data al/0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,
c     *0.,1.,1.,2.,0.,2.,3.,1.,3.,1.,4.,0.,18*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,
c     *.5,1.5,.5,2.5,.5,1.5,3.5,1.5,2.5,0.5,4.5,0.,18*0./
c      data en/-48.3,-39.4,-38.2,-29.3,-26.4,-26.9,-20.,-14.,-14.,-14.,
c     *-47.9,-37.5,-36.1,-27.3,-22.1,-23.9,-25.0,-17.0,-12.,-10.,-7.,
c     *19*0./
c      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.8,1.8,1.8,1.8,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,1.7,1.6,1.5,1.4,19*0./
c      data h1,h2,n,ht,rt/.015,.125,794,.02,.39/

cccccccccccccc Zr90 ccccccccccccccccccc
c      Data ln/26/,l/11,15,0/,alo/2,4,2,6,2,4,8,6,4,2,0,
c     ,2,4,2,6,2,4,
c     ,8,6,4,2,10,0,0,0,0,14*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,
c     ,0.,1.,1.,2.,0.,2.,
c     ,3.,3.,1.,1.,4.,2.,0.,4.,2.,14*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,
c     ,.5,1.5,.5,2.5,.5,1.5,
c     ,3.5,2.5,1.5,.5,4.5,2.5,.5,3.5,1.5,14*0./
c      data iter,irf,irf1/1,2,0/
c      data en/-45.,-36.,-34.,-27.,-21.,-22.,
c     ,-17.,-10.,-10.,-8.,-6.,
c     ,-55.,-45.,-44.,-34.,-29.,
c     ,-31.,-23.,-18.,-17.,-16.,-12.,-7.,-5.,-5.,-4.,14*0./
c      data h1,h2,n,ht,rt/.045,.55,200,.06,.9/
c      data h1,h2,n,ht,rt/.02,.125,700,06,.9/
c      data bf/27*2.75,13*0./

cccccccccccccc Zr92 ccccccccccccccccccc
c      Data ln/26/,l/11,15,0/,alo/2,4,2,6,2,4,8,6,4,2,0,
c     ,2,4,2,6,2,4,
c     ,8,6,4,2,10,2,0,0,0,14*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,
c     ,0.,1.,1.,2.,0.,2.,
c     ,3.,3.,1.,1.,4.,2.,0.,4.,2.,14*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,
c     ,.5,1.5,.5,2.5,.5,1.5,
c     ,3.5,2.5,1.5,.5,4.5,2.5,.5,3.5,1.5,14*0./
c      data iter,irf,irf1/1,2,0/
c      data en/-45.,-37.,-35.,-27.,-22.,-23.,
c     ,-17.,-10.,-11.,-9.,-7.,
c     ,-55.,-45.,-44.,-34.,-30.,
c     ,-31.,-23.,-18.,-18,-16.,-12.,-7.,-5.,-5.,-4.,14*0./
c      data h1,h2,n,ht,rt/.045,.55,200,.06,.9/
c      data bf/27*2.75,13*0./

cccccccccccccc Zr94 ccccccccccccccccccc
c      Data ln/26/,l/11,15,0/,alo/2,4,2,6,2,4,8,6,4,2,0,
c     ,2,4,2,6,2,4,
c     ,8,6,4,2,10,4,0,0,0,14*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,
c     ,0.,1.,1.,2.,0.,2.,
c     ,3.,3.,1.,1.,4.,2.,0.,4.,2.,14*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,
c     ,.5,1.5,.5,2.5,.5,1.5,
c     ,3.5,2.5,1.5,.5,4.5,2.5,.5,3.5,1.5,14*0./
c      data iter,irf,irf1/1,2,0/
c      data en/-46.,-38.,-35.,-28.,-23.,-23.,
c     ,-18.,-11.,-12.,-10.,-10.,
c     ,-56.,-45.,-44.,-34.,-30.,
c     ,-31.,-23.,-18.,-18,-16.,-12.,-7.,-5.,-5.,-4.,14*0./
c      data h1,h2,n,ht,rt/.045,.55,200,.06,.9/
c      data bf/27*2.75,13*0./

cccccccccccccc Zr96 ccccccccccccccccccc
c      Data ln/26/,l/11,15,0/,alo/2,4,2,6,2,4,8,6,4,2,0,
c     ,2,4,2,6,2,4,
c     ,8,6,4,2,10,6,0,0,0,14*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,
c     ,0.,1.,1.,2.,0.,2.,
c     ,3.,3.,1.,1.,4.,2.,0.,4.,2.,14*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,
c     ,.5,1.5,.5,2.5,.5,1.5,
c     ,3.5,2.5,1.5,.5,4.5,2.5,.5,3.5,1.5,14*0./
c      data iter,irf,irf1/1,2,0/
c      data en/-47.,-38.,-36.,-29.,-24.,-24.,
c     ,-19.,-12.,-13.,-11.,-9.,
c     ,-56.,-45.,-44.,-34.,-30.,
c     ,-31.,-23.,-18.,-18,-17.,-12.,-7.,-5.,-5.,-5.,14*0./
c      data h1,h2,n,ht,rt/.045,.55,200,.06,.9/
c      data bf/27*2.75,13*0./

cccccccccccccc Zr98 ccccccccccccccccccc
c      Data ln/26/,l/11,15,0/,alo/2,4,2,6,2,4,8,6,4,2,0,
c     ,2,4,2,6,2,4,
c     ,8,6,4,2,10,6,2,0,0,14*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,
c     ,0.,1.,1.,2.,0.,2.,
c     ,3.,3.,1.,1.,4.,2.,0.,4.,2.,14*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,
c     ,.5,1.5,.5,2.5,.5,1.5,
c     ,3.5,2.5,1.5,.5,4.5,2.5,.5,3.5,1.5,14*0./
c      data iter,irf,irf1/1,2,0/
c      data en/-48.,-39.,-36.,-29.,-25.,-24.,
c     ,-19.,-12.,-14.,-11.,-10.,
c     ,-56.,-45.,-44.,-34.,-31.,
c     ,-31.,-23.,-18.,-18,-16.,-12.,-7.,-5.,-5.,-5.,14*0./
c      data h1,h2,n,ht,rt/.045,.55,200,.06,.9/
c      data bf/27*2.75,13*0./

cccccccccccccc Zr100 ccccccccccccccccccc
c      Data ln/26/,l/11,15,0/,alo/2,4,2,6,2,4,8,6,4,2,0,
c     ,2,4,2,6,2,4,
c     ,8,6,4,2,10,6,2,2,0,14*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,
c     ,0.,1.,1.,2.,0.,2.,
c     ,3.,3.,1.,1.,4.,2.,0.,4.,2.,14*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,
c     ,.5,1.5,.5,2.5,.5,1.5,
c     ,3.5,2.5,1.5,.5,4.5,2.5,.5,3.5,1.5,14*0./
c      data iter,irf,irf1/1,2,0/
c      data en/-49.,-41.,-38.,-32.,-27.,-28.,
c     ,-22.,-17.,-17.,-15.,-13.,
c     ,-54.,-44.,-43.,-34.,-31.,
c     ,-31.,-23.,-18.,-19,-17.,-13.,-8.,-6.,-5.,-5.,14*0./
c      data h1,h2,n,ht,rt/.045,.55,200,.06,.9/
c      data bf/27*2.75,13*0./

cccccccccccccc Zr102 ccccccccccccccccccc
c      Data ln/26/,l/11,15,0/,alo/2,4,2,6,2,4,8,6,4,2,0,
c     ,2,4,2,6,2,4,
c     ,8,6,4,2,10,6,2,4,0,14*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,
c     ,0.,1.,1.,2.,0.,2.,
c     ,3.,3.,1.,1.,4.,2.,0.,4.,2.,14*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,
c     ,.5,1.5,.5,2.5,.5,1.5,
c     ,3.5,2.5,1.5,.5,4.5,2.5,.5,3.5,1.5,14*0./
c      data iter,irf,irf1/1,2,0/
c      data en/-49.,-41.,-38.,-32.,-27.,-28.,
c     ,-22.,-17.,-17.,-15.,-13.,
c     ,-54.,-44.,-43.,-34.,-31.,
c     ,-31.,-23.,-18.,-19,-17.,-13.,-8.,-6.,-5.,-5.,14*0./
c      data h1,h2,n,ht,rt/.045,.55,200,.06,.9/
c      data bf/27*2.75,13*0./

cccccccccccccc Zr104 ccccccccccccccccccc
c      Data ln/26/,l/11,15,0/,alo/2,4,2,6,2,4,8,6,4,2,0,
c     ,2,4,2,6,2,4,
c     ,8,6,4,2,10,6,2,6,0,14*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,
c     ,0.,1.,1.,2.,0.,2.,
c     ,3.,3.,1.,1.,4.,2.,0.,4.,2.,14*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,
c     ,.5,1.5,.5,2.5,.5,1.5,
c     ,3.5,2.5,1.5,.5,4.5,2.5,.5,3.5,1.5,14*0./
c      data iter,irf,irf1/1,2,0/
c      data en/-49.,-41.,-38.,-32.,-27.,-28.,
c     ,-22.,-17.,-17.,-15.,-13.,
c     ,-54.,-44.,-43.,-34.,-31.,
c     ,-31.,-23.,-18.,-19,-17.,-13.,-8.,-6.,-5.,-5.,14*0./
c      data h1,h2,n,ht,rt/.045,.55,200,.06,.9/
c      data bf/27*2.75,13*0./

cccccccccccccc Zr106 ccccccccccccccccccc
c      Data ln/26/,l/11,15,0/,alo/2,4,2,6,2,4,8,6,4,2,0,
c     ,2,4,2,6,2,4,
c     ,8,6,4,2,10,6,2,8,0,14*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,
c     ,0.,1.,1.,2.,0.,2.,
c     ,3.,3.,1.,1.,4.,2.,0.,4.,2.,14*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,
c     ,.5,1.5,.5,2.5,.5,1.5,
c     ,3.5,2.5,1.5,.5,4.5,2.5,.5,3.5,1.5,14*0./
c      data iter,irf,irf1/1,2,0/
c      data en/-49.,-41.,-38.,-32.,-27.,-28.,
c     ,-22.,-17.,-17.,-15.,-13.,
c     ,-54.,-44.,-43.,-34.,-31.,
c     ,-31.,-23.,-18.,-19,-17.,-13.,-8.,-6.,-5.,-5.,14*0./
c      data h1,h2,n,ht,rt/.045,.55,200,.06,.9/
c      data bf/27*2.75,13*0./

cccccccccccccc Zr108 ccccccccccccccccccc
c      Data ln/26/,l/11,15,0/,alo/2,4,2,6,2,4,8,6,4,2,0,
c     ,2,4,2,6,2,4,
c     ,8,6,4,2,10,6,2,8,2,14*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,
c     ,0.,1.,1.,2.,0.,2.,
c     ,3.,3.,1.,1.,4.,2.,0.,4.,2.,14*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,
c     ,.5,1.5,.5,2.5,.5,1.5,
c     ,3.5,2.5,1.5,.5,4.5,2.5,.5,3.5,1.5,14*0./
c      data iter,irf,irf1/1,2,0/
c      data en/-49.,-41.,-40.,-33.,-29.,-29.,
c     ,-24.,-18.,-18.,-16.,-14.,
c     ,-54.,-44.,-42.,-34.,-30.,
c     ,-30.,-24.,-18.,-19,-16.,-13.,-8.,-6.,-6.,-5.,14*0./
c      data h1,h2,n,ht,rt/.045,.55,200,.06,.9/
c      data bf/27*2.75,13*0./

cccccccccccccc Zr110 ccccccccccccccccccc
c      Data ln/26/,l/11,15,0/,alo/2,4,2,6,2,4,8,6,4,2,0,
c     ,2,4,2,6,2,4,
c     ,8,6,4,2,10,6,2,8,4,14*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,
c     ,0.,1.,1.,2.,0.,2.,
c     ,3.,3.,1.,1.,4.,2.,0.,4.,2.,14*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,
c     ,.5,1.5,.5,2.5,.5,1.5,
c     ,3.5,2.5,1.5,.5,4.5,2.5,.5,3.5,1.5,14*0./
c      data iter,irf,irf1/1,2,0/
c      data en/-49.,-41.,-40.,-33.,-29.,-29.,
c     ,-24.,-18.,-18.,-16.,-14.,
c     ,-54.,-44.,-42.,-34.,-30.,
c     ,-30.,-24.,-18.,-19,-16.,-13.,-8.,-6.,-6.,-5.,14*0./
c      data h1,h2,n,ht,rt/.045,.55,200,.06,.9/
c      data bf/27*2.75,13*0./

cccccccccccccc Sn114 ccccccccccccccccccc
c      Data ln/26/,l/11,15,0/,alo/2,4,2,6,2,4,8,6,4,2,10,
c     ,2,4,2,6,2,4,
c     ,8,6,4,2,10,6,8,2,4,14*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,
c     ,0.,1.,1.,2.,0.,2.,
c     ,3.,3.,1.,1.,4.,2.,4.,0.,2.,14*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,
c     ,.5,1.5,.5,2.5,.5,1.5,
c     ,3.5,2.5,1.5,.5,4.5,2.5,3.5,.5,1.5,14*0./
c      data iter,irf,irf1/1,2,0/
c      data en/-44.,-35.,-34.,-26.,-22.,-24.,
c     ,-19.,-14.,-13.,-11.,-9.,
c     ,-57.,-48.,-47.,-38.,-33.,
c     ,-35.,-28.,-22.,-21.5,-19.,-16.,-14.,-11.,-9.,-6.,14*0./
c      data h1,h2,n,ht,rt/.045,.55,200,.06,.9/
c      data bf/27*2.75,13*0./
cccccccccccccc Sn132 ccccccccccccccccccc
c      Data ln/27/,l/11,16,0/,alo/2,4,2,6,2,4,8,6,4,2,10,
c     ,2,4,2,6,2,4,
c     ,8,6,4,2,10,8,6,12,4,2,0,12*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,
c     ,0.,1.,1.,2.,0.,2.,
c     ,3.,3.,1.,1.,4.,4.,2.,5.,2.,0.,13*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,
c     ,.5,1.5,.5,2.5,.5,1.5,
c     ,3.5,2.5,1.5,.5,4.5,3.5,2.5,5.5,1.5,.5,13*0./
c      data iter,irf,irf1/1,2,0/
c      data en/-49.,-42.,-41.,-33.,-28.,-31.,
c     ,-24.,-20.,-19.,-17.,-15.,
c     ,-55.,-47.,-46.,-38.,-32.,-36.,
c     ,-28.,-24.,-22.,-20.,-17.,-11.,-11.,-7.,-9.,-10.,0,12*0./
c      data h1,h2,n,ht,rt/.045,.55,200,.06,.9/
c      data bf/27*2.75,13*0./
cccccccccccccc Pb208 ccccccccccccccccccc
c      Data ln/38/,l/16,22,0/,alo/2,4,2,6,2,4,8,6,4,2,10,8,6,12,4,2,
c     ,2,4,2,6,2,4,
c     ,8,6,4,2,10,8,6,12,4,2,10,8,14,4,6,2,0,0/
c      data al/0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,4.,2.,5.,2.,0.,
c     ,0.,1.,1.,2.,0.,2.,
c     ,3.,3.,1.,1.,4.,4.,2.,5.,2.,0.,5.,3.,6.,1.,3.,1.,0.,0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,
c     ,3.5,2.5,5.5,1.5,.5,
c     ,.5,1.5,.5,2.5,.5,1.5,
c     ,3.5,2.5,1.5,.5,4.5,3.5,2.5,5.5,1.5,.5,4.5,3.5,6.5,1.5,2.5,.5,2*0./
c      data iter,irf,irf1/1,2,0/
c      data en/-53.,-47.,-46.,-39.,-38.,-34.,-30.,-28.,-23.,-22.,
c     ,-20.,-17.,-12.,-10.,-11.,-9.,-63.,
c     ,-56.,-56.,-48.,-47.,-44.,-39.,-37.,-33.,-32.,-29.,
c     ,-26.,-22.,-19.,-20.,-19.,-14.,-12.5,-9.,-8.,-10.5,-7.,0.,0./
c      data h1,h2,n,ht,rt/.045,.55,200,.06,.9/
c      data bf/38*2.75,0.,0./
cccccccccccccc Pb208 Lambda ccccccccccccccccccc
c      Data ln/39/,l/16,22,1/,alo/2,4,2,6,2,4,8,6,4,2,10,8,6,12,4,2,
c     ,2,4,2,6,2,4,
c     ,8,6,4,2,10,8,6,12,4,2,10,8,14,4,6,2,1,0/
c      data al/0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,4.,2.,5.,2.,0.,
c     ,0.,1.,1.,2.,0.,2.,
c     ,3.,3.,1.,1.,4.,4.,2.,5.,2.,0.,5.,3.,6.,1.,3.,1.,0.,0./
c      data pmn(1),pmn(2),pml/938.2796,939.573,1115.6/
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,
c     ,3.5,2.5,5.5,1.5,.5,
c     ,.5,1.5,.5,2.5,.5,1.5,3.5,2.5,
c     ,1.5,.5,4.5,3.5,2.5,5.5,1.5,.5,4.5,3.5,6.5,1.5,2.5,.5,.5,0./
c      data iter,irf,irf1/1,2,0/
c      data en/-53.,-47.,-46.,-39.,-38.,-34.,-30.,-28.,-23.,-22.,
c     ,-20.,-17.,-12.,-10.,-11.,-9.,-63.,
c     ,-56.,-56.,-48.,-47.,-44.,-39.,-37.,-33.,-32.,-29.,
c     ,-26.,-22.,-19.,-20.,-19.,-14.,-12.5,-9.,-8.,-10.5,-7.,-20.,0./
c      data h1,h2,n,ht,rt/.045,.55,200,.06,.9/
c      data tl/-755.6,1263.,1263.,0.,4*0.,0.,0.,0.,5./
c      data tl/12*0./
c      data bf/39*2.75,0./
c      data t/-1169.9,585.6,-27.1,9331.1,.34,105.,1.,1.,
c     ,4*0./

cccccccccccccc Sm152 (Z=62) ccccccccccccccccccc
c      Data ln/30/,l/13,17,0/,alo/2,4,2,6,2,4,8,6,4,2,10,8,4,
c     ,2,4,2,6,2,4,8,6,4,2,10,8,6,12,4,2,8,10*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,4.,2.,
c     ,0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,4.,2.,5.,2.,0.,5.,10*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,3.5,2.5,
c     ,.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,3.5,2.5,5.5,1.5,.5,4.5,
c     ,10*0./
c      data iter,irf,irf1/1,2,0/
c      data en/-42.,-36.,-36.,-30.,-26.,-28.,-22.,-19.,-17.,-16.,
c     ,-15.,-10.,-7.,
c     ,-49.,-43.,-43.,-37.,-33.,-35.,-29.,-26.,-24.,-23.,
c     ,-21.,-16.,-15.,-13.,-13.,-12.,-5.,10*0./
c      data h1,h2,n,ht,rt/.045,.55,200,.06,.9/
c      data bf/30*2.75,10*0./
cccccccccccccc Sm152 (Z=62) unocc ccccccccccccccccccc
c      Data ln/38/,l/16,22,0/,alo/2,4,2,6,2,4,8,6,4,2,10,8,4,0,0,0,
c     ,2,4,2,6,2,4,8,6,4,2,10,8,6,12,4,2,8,0,0,0,0,0,2*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,4.,2.,5.,2.,0.,
c     ,0.,1.,1.,2.,0.,2.,
c     ,3.,3.,1.,1.,4.,4.,2.,5.,2.,0.,5.,3.,6.,1.,3.,1.,0.,0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,
c     ,3.5,2.5,5.5,1.5,.5,
c     ,.5,1.5,.5,2.5,.5,1.5,
c     ,3.5,2.5,1.5,.5,4.5,3.5,2.5,5.5,1.5,.5,4.5,3.5,6.5,1.5,2.5,.5,2*0./
c      data iter,irf,irf1/1,2,0/
c      data en/-42.,-36.,-36.,-30.,-26.,-28.,-22.,-19.,-17.,-16.,
c     ,-15.,-10.,-7.,-6.,-5.,-5.,
c     ,-49.,-43.,-43.,-37.,-33.,-35.,-29.,-26.,-24.,-23.,
c     ,-21.,-16.,-15.,-13.,-13.,-12.,-5.,-6.,-4.,-3.,-3.,-2.,2*0./
c      data h1,h2,n,ht,rt/.045,.55,200,.06,.9/
c      data bf/38*2.75,2*0./

cccccccccccccc Sm153 (Z=62) ccccccccccccccccccc
c      Data ln/30/,l/13,17,0/,alo/2,4,2,6,2,4,8,6,4,2,10,8,4,
c     ,2,4,2,6,2,4,8,6,4,2,10,8,6,12,4,2,9,10*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,4.,2.,
c     ,0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,4.,2.,5.,2.,0.,5.,10*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,3.5,2.5,
c     ,.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,3.5,2.5,5.5,1.5,.5,4.5,
c     ,10*0./
c      data iter,irf,irf1/1,2,0/
c      data en/-42.,-36.,-36.,-30.,-26.,-28.,-22.,-19.,-17.,-16.,
c     ,-15.,-10.,-7.,
c     ,-49.,-43.,-43.,-37.,-33.,-35.,-29.,-26.,-24.,-23.,
c     ,-21.,-16.,-15.,-13.,-13.,-12.,-5.,10*0./
c      data h1,h2,n,ht,rt/.045,.55,200,.06,.9/
c      data bf/30*2.75,10*0./
cccccccccccccc Sm153 (Z=62) unocc ccccccccccccccccccc
c      Data ln/38/,l/16,22,0/,alo/2,4,2,6,2,4,8,6,4,2,10,8,4,0,0,0,
c     ,2,4,2,6,2,4,8,6,4,2,10,8,6,12,4,2,9,0,0,0,0,0,2*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,4.,2.,5.,2.,0.,
c     ,0.,1.,1.,2.,0.,2.,
c     ,3.,3.,1.,1.,4.,4.,2.,5.,2.,0.,5.,3.,6.,1.,3.,1.,0.,0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,
c     ,3.5,2.5,5.5,1.5,.5,
c     ,.5,1.5,.5,2.5,.5,1.5,
c     ,3.5,2.5,1.5,.5,4.5,3.5,2.5,5.5,1.5,.5,4.5,3.5,6.5,1.5,2.5,.5,2*0./
c      data iter,irf,irf1/1,2,0/
c      data en/-42.,-36.,-36.,-30.,-26.,-28.,-22.,-19.,-17.,-16.,
c     ,-15.,-10.,-7.,-6.,-5.,-5.,
c     ,-49.,-43.,-43.,-37.,-33.,-35.,-29.,-26.,-24.,-23.,
c     ,-21.,-16.,-15.,-13.,-13.,-12.,-5.,-6.,-4.,-3.,-3.,-2.,2*0./
c      data h1,h2,n,ht,rt/.045,.55,200,.06,.9/
c      data bf/38*2.75,2*0./


cccccccccccccc Gd158 (Z=64) ccccccccccccccccccc
c      Data ln/31/,l/13,18,0/,alo/2,4,2,6,2,4,8,6,4,2,10,8,6,
c     ,2,4,2,6,2,4,8,6,4,2,10,8,6,12,4,2,10,2,9*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,4.,2.,
c     ,0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,4.,2.,5.,2.,0.,5.,3.,9*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,3.5,2.5,
c     ,.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,3.5,2.5,5.5,1.5,.5,4.5,
c     ,3.5,9*0./
c      data iter,irf,irf1/1,2,0/
c      data en/-42.,-36.,-36.,-30.,-26.,-28.,-22.,-19.,-17.,-16.,
c     ,-15.,-10.,-7.,
c     ,-50.,-44.,-43.,-37.,-34.,-35.,-29.,-26.,-25.,-24.,
c     ,-21.,-17.,-16.,-13.,-13.,-13.,-6.,-6.,9*0./
c      data h1,h2,n,ht,rt/.045,.55,200,.06,.9/
c      data bf/31*2.75,9*0./
cccccccccccccc Gd158 (Z=64) unocc ccccccccccccccccccc
c      Data ln/38/,l/16,22,0/,alo/2,4,2,6,2,4,8,6,4,2,10,8,6,0,0,0,
c     ,2,4,2,6,2,4,8,6,4,2,10,8,6,12,4,2,10,2,0,0,0,0,2*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,4.,2.,5.,2.,0.,
c     ,0.,1.,1.,2.,0.,2.,
c     ,3.,3.,1.,1.,4.,4.,2.,5.,2.,0.,5.,3.,6.,1.,3.,1.,0.,0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,
c     ,3.5,2.5,5.5,1.5,.5,
c     ,.5,1.5,.5,2.5,.5,1.5,
c     ,3.5,2.5,1.5,.5,4.5,3.5,2.5,5.5,1.5,.5,4.5,3.5,6.5,1.5,2.5,.5,2*0./
c      data iter,irf,irf1/1,2,0/
c      data en/-42.,-36.,-36.,-30.,-26.,-28.,-22.,-19.,-17.,-16.,
c     ,-15.,-10.,-7.,-6.,-5.,-5.,
c     ,-50.,-44.,-43.,-37.,-34.,-35.,-29.,-26.,-25.,-24.,
c     ,-21.,-17.,-16.,-13.,-13.,-13.,-6.,-6.,-4.,-3.,-3.,-2.,2*0./
c      data h1,h2,n,ht,rt/.045,.55,200,.06,.9/
c      data bf/38*2.75,2*0./

cccccccccccccc Dy162 (Z=66) ccccccccccccccccccc
c      Data ln/32/,l/14,18,0/,alo/2,4,2,6,2,4,8,6,4,2,10,8,6,2,
c     ,2,4,2,6,2,4,8,6,4,2,10,8,6,12,4,2,10,4,8*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,4.,2.,5.,
c     ,0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,4.,2.,5.,2.,0.,5.,3.,8*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,3.5,2.5,5.5,
c     ,.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,3.5,2.5,5.5,1.5,.5,4.5,
c     ,3.5,8*0./
c      data iter,irf,irf1/1,2,0/
c      data en/-41.,-36.,-35.,-29.,-26.,-28.,-22.,-19.,-17.,-16.,
c     ,-15.,-10.,-7.,-6.,
c     ,-50.,-44.,-43.,-37.,-34.,-36.,-30.,-27.,-25.,-24.,
c     ,-22.,-17.,-16.,-14.,-14.,-14.,-7.,-7.,8*0./
c      data h1,h2,n,ht,rt/.045,.55,200,.06,.9/
c      data bf/32*2.75,8*0./
cccccccccccccc Dy162 (Z=66) unocc ccccccccccccccccccc
c      Data ln/38/,l/16,22,0/,alo/2,4,2,6,2,4,8,6,4,2,10,8,6,2,0,0,
c     ,2,4,2,6,2,4,8,6,4,2,10,8,6,12,4,2,10,4,0,0,0,0,2*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,4.,2.,5.,2.,0.,
c     ,0.,1.,1.,2.,0.,2.,
c     ,3.,3.,1.,1.,4.,4.,2.,5.,2.,0.,5.,3.,6.,1.,3.,1.,0.,0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,
c     ,3.5,2.5,5.5,1.5,.5,
c     ,.5,1.5,.5,2.5,.5,1.5,
c     ,3.5,2.5,1.5,.5,4.5,3.5,2.5,5.5,1.5,.5,4.5,3.5,6.5,1.5,2.5,.5,2*0./
c      data iter,irf,irf1/1,2,0/
c      data en/-41.,-36.,-35.,-29.,-26.,-28.,-22.,-19.,-17.,-16.,
c     ,-15.,-10.,-7.,-6.,-5.,-5.,
c     ,-50.,-44.,-43.,-37.,-34.,-36.,-30.,-27.,-25.,-24.,
c     ,-22.,-17.,-16.,-14.,-14.,-14.,-7.,-7.,-4.,-3.,-3.,-2.,2*0./
c      data h1,h2,n,ht,rt/.045,.55,200,.06,.9/
c      data bf/38*2.75,2*0./

cccccccccccccc Er166 (Z=68) ccccccccccccccccccc
c      Data ln/32/,l/14,18,0/,alo/2,4,2,6,2,4,8,6,4,2,10,8,6,4,
c     ,2,4,2,6,2,4,8,6,4,2,10,8,6,12,4,2,10,6,8*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,4.,2.,5.,
c     ,0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,4.,2.,5.,2.,0.,5.,3.,8*0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,3.5,2.5,5.5,
c     ,.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,3.5,2.5,5.5,1.5,.5,4.5,
c     ,3.5,8*0./
c      data iter,irf,irf1/1,2,0/
c      data en/-42.,-36.,-36.,-30.,-27,-28.,-23.,-20.,-17.,-16.,
c     ,-15.,-10.,-8.,-7.,
c     ,-50.,-44.,-43.,-37.,-34.,-36.,-30.,-27.,-25.,-24.,
c     ,-22.,-17.,-16.,-14.,-14.,-14.,-7.,-7.,8*0./
c      data h1,h2,n,ht,rt/.045,.55,200,.06,.9/
c      data bf/32*2.75,8*0./
cccccccccccccc Er166 (Z=68) unocc ccccccccccccccccccc
c      Data ln/38/,l/16,22,0/,alo/2,4,2,6,2,4,8,6,4,2,10,8,6,4,0,0,
c     ,2,4,2,6,2,4,8,6,4,2,10,8,6,12,4,2,10,6,0,0,0,0,2*0/
c      data al/0.,1.,1.,2.,0.,2.,3.,3.,1.,1.,4.,4.,2.,5.,2.,0.,
c     ,0.,1.,1.,2.,0.,2.,
c     ,3.,3.,1.,1.,4.,4.,2.,5.,2.,0.,5.,3.,6.,1.,3.,1.,0.,0./
c      data aj/.5,1.5,.5,2.5,.5,1.5,3.5,2.5,1.5,.5,4.5,
c     ,3.5,2.5,5.5,1.5,.5,
c     ,.5,1.5,.5,2.5,.5,1.5,
c     ,3.5,2.5,1.5,.5,4.5,3.5,2.5,5.5,1.5,.5,4.5,3.5,6.5,1.5,2.5,.5,2*0./
c      data iter,irf,irf1/1,2,0/
c      data en/-42.,-36.,-36.,-30.,-27,-28.,-23.,-20.,-17.,-16.,
c     ,-15.,-10.,-8.,-7.,-6.,-6.,
c     ,-50.,-44.,-43.,-37.,-34.,-36.,-30.,-27.,-25.,-24.,
c     ,-22.,-17.,-16.,-14.,-14.,-14.,-7.,-7.,-4.,-3.,-3.,-2.,2*0./
c      data h1,h2,n,ht,rt/.045,.55,200,.06,.9/
c      data bf/38*2.75,2*0./

cccccc!!!Ca!!!cccccc
c      data h1,h2,n,ht,rt/.03,.3,397,.02,.45/
cccccc��� Fix for not-smooth solutions 08.05.19cccccc
c      data h1,h2,n,ht,rt/.025,.25,397,.05,.3/

c      data h1,h2,n,ht,rt/.03,.25,397,.02,.39/

      data pmn(1),pmn(2),pml/938.2796,939.573,0./
c      data pmn(1),pmn(2),pml/938.2796,939.573,1115.6/

      data tl/15*0.0/,hypparamet/""/
ccccccccccccccc  1988  ccccccccccccccccccccccccccccccccccccc
c	data tl/-201.1,2*0.,0.,4*0.,1.0,0.,0.,1./
ccccccccccccccc  R3    ccccccccccccccccccccccccccccccccccccc
c      data tl/-628.5,179.5,-43.2,7539.,4*0.,1.,0.,0.,1./
ccccccccccccccc  R13   ccccccccccccccccccccccccccccccccccccc
c      data tl/-280.7,-32.5,32.5,2000.,4*0.,1.,0.,0.,1./
ccccccccccccccc  YBZ6  ccccccccccccccccccccccccccccccccccccc
c		data tl/-352.29,100.4,79.6,2000.,4*0.0,1.,0.,0.,1./
ccccccccccccccc  SKSH1 no CSB cccccccccccccccccccccccccccccccccccc
c      data tl/-176.5,-35.8,44.1,0.0,4*0.,1.,0.,0.,1.,3*0. /
c     *,hypparamet/"_SkSH1"/
ccccccccccccccc  SKSH1 + CSB cccccccccccccccccccccccccccccccccccc
c		data tl/-176.5,-35.8,44.1,0.0,4*0.,1.,0.,0.,1.,-1.,2*0./
ccccccccccccccc  YBZ5 no CSB  cccccccccccccccccccccccccccccccccccc
c	data tl/-298.12,23.14,-23.14,2000.,4*0.,1.,2*0.,1.,3*0./
c    *,hypparamet/"_YBZ5"/
ccccccccccccccc  YBZ1  ccccccccccccccccccccccccccccccccccccc
c	data tl/-330.154,67.61,37.39,2000.,4*0.0,1.,0.,0.,1.,3*0./
ccccccccccccccc  YBZ2  ccccccccccccccccccccccccccccccccccccc
c	data tl/-391.8,56.95,48.05,3000,-0.085,3*0.0,1.,0.,0.,1./
cccccccccccccccc  LY V  cccccccccccccccccccccccccccc
C      data tl/-542.2,58.,8.,1383.3,-.1536,0.,0.,.1077,.3333,62.,0.,4*0./
ccccccccccccccc  LY V modified cccccccccccccccccccc
c      data tl/-542.2,58.,8.,1383.3,-.1536,0.,0.,.1077,.3333,4.7,0.,4*0./
c     *,hypparamet/"_LY5r"/
cccccccccccccccc  LY IV  ccccccccccccccccccccccccccc
c      data tl/-542.5,56.,8.,1387.9,-.1534,0.,0.,.1074,.3333,0.,0.,0./
cccccccccccccccc LY I no CSB  ccccccccccccccccccccccccccc
c      data tl/-476.0,42.,23.,1514.1,-.0452,0.,0.,-0.280,.3333,0.,5*0./
c     *,hypparamet/"_LY1"/
cccccccccccccccc LY I, CSB I ccccccccccccccccccccccccccc
c      data tl/-476.0,42.,23.,1514.1,-.0452,0.,0.,-0.280,.3333,0.,2*0.,
c     *2.266,-0.009,0./,hypparamet/"_LY1_CSB1"/
cccccccccccccccc LY I, CSB II ccccccccccccccccccccccccccc
c      data tl/-476.0,42.,23.,1514.1,-.0452,0.,0.,-0.280,.3333,0.,2*0.,
c     *3.765,0.078,0./,hypparamet/"_LY1_CSB2"/
cccccccccccccccc LY I, CSB III ccccccccccccccccccccccccccc
c      data tl/-476.0,42.,23.,1514.1,-.0452,0.,0.,-0.280,.3333,0.,2*0.,
c     *-5.611,3.648,0./,hypparamet/"_LY1_CSB3"/
cccccccccccccccc LY I, CSB IV ccccccccccccccccccccccccccc
c      data tl/-476.0,42.,23.,1514.1,-.0452,0.,0.,-0.280,.3333,0.,2*0.,
c     *-6.595,1.463,0./,hypparamet/"_LY1_CSB4"/
cccccccccccccccc LY I, CSB V ccccccccccccccccccccccccccc
c      data tl/-476.0,42.,23.,1514.1,-.0452,0.,0.,-0.280,.3333,0.,2*0.,
c     *-6.828,8.151,0./,hypparamet/"_LY1_CSB5"/
cccccccccccccccc LY I, CSB VI ccccccccccccccccccccccccccc
c      data tl/-476.0,42.,23.,1514.1,-.0452,0.,0.,-0.280,.3333,0.,2*0.,
c     *1.442,-0.206,0./,hypparamet/"_LY1_CSB6"/
cccccccccccccccc LY I, CSB VII ccccccccccccccccccccccccccc
c      data tl/-476.0,42.,23.,1514.1,-.0452,0.,0.,-0.280,.3333,0.,2*0.,
c     *0.520,0.124,0./,hypparamet/"_LY1_CSB7"/
cccccccccccccccc SLL4*, density-dependent, no CSB cccccccccccccccccccccccccccccccccccc
c      data tl/-326.0,62.0,20.,1880.,4*0.0,1.,0.,0.,0.,0.,0.,0./
c     *,hypparamet/"_SLL4'"/
cccccccccccccccc SLL4*, density-dependent, CSB I cccccccccccccccccccccccccccccccccccc
c      data tl/-326.0,62.0,20.,1880.,4*0.0,1.,0.,0.,0.,2.266,-0.009,0./
c     *,hypparamet/"_SLL4'_dens_CSB1"/
cccccccccccccccc SLL4*, density-dependent, CSB II cccccccccccccccccccccccccccccccccccc
c      data tl/-326.0,62.0,20.,1880.,4*0.0,1.,0.,0.,0.,3.765,0.078,0./
c     *,hypparamet/"_SLL4'_dens_CSB2"/
cccccccccccccccc SLL4*, density-dependent, CSB III cccccccccccccccccccccccccccccccccccc
c      data tl/-326.0,62.0,20.,1880.,4*0.0,1.,0.,0.,0.,-5.611,3.648,0./
c     *,hypparamet/"_SLL4'_dens_CSB3"/
cccccccccccccccc SLL4*, density-dependent, CSB IV cccccccccccccccccccccccccccccccccccc
c      data tl/-326.0,62.0,20.,1880.,4*0.0,1.,0.,0.,0.,-6.595,1.463,0./
c     *,hypparamet/"_SLL4'_dens_CSB4"/
cccccccccccccccc SLL4*, density-dependent, CSB V cccccccccccccccccccccccccccccccccccc
c      data tl/-326.0,62.0,20.,1880.,4*0.0,1.,0.,0.,0.,-6.828,8.151,0./
c     *,hypparamet/"_SLL4'_dens_CSB5"/
cccccccccccccccc SLL4*, density-dependent, CSB VI cccccccccccccccccccccccccccccccccccc
c      data tl/-326.0,62.0,20.,1880.,4*0.0,1.,0.,0.,0.,1.442,-0.206,0./
c     *,hypparamet/"_SLL4'_dens_CSB6"/
cccccccccccccccc SLL4*, density-dependent, CSB VII cccccccccccccccccccccccccccccccccccc
c      data tl/-326.0,62.0,20.,1880.,4*0.0,1.,0.,0.,0.,0.520,0.124,0./
c     *,hypparamet/"_SLL4'_dens_CSB7"/
cccccccccccccccc SLL4*, triple forces, no CSB cccccccccccccccccccccccccccccccccccc
c      data tl/-326.0,62.0,20.,1880.,4*0.0,1.,0.,0.,1.,0.,0.,0./
c     *,hypparamet/"_SLL4'"/
cccccccccccccccc SLL4* + CSB ccccccccccccccccccccccccccccccccccccc
c      data tl/-326.0,62.0,20.,1880.,4*0.0,1.,0.,0.,1.,10.,00.,00./
cccccccccccccccc SLL4* + singlet ccccccccccccccccccccccccccccccccccccc
c      data tl/-326.0,62.0,20.,1880.,4*0.0,1.,0.,0.,1.,10.97,-13.7,00./
cccccccccccccccc SLL4* + triplet ccccccccccccccccccccccccccccccccccccc
c      data tl/-326.0,62.0,20.,1880.,4*0.0,1.,0.,0.,1.,-5.058,4.843,0./
cccccccccccccccc SLL4* + CSB I ccccccccccccccccccccccccccccccccccccc
c      data tl/-326.0,62.0,20.,1880.,4*0.0,1.,0.,0.,1.,2.266,-0.009,0./
c     *,hypparamet/"_SLL4'_CSB1"/
cccccccccccccccc SLL4* + CSB II ccccccccccccccccccccccccccccccccccccc
c      data tl/-326.0,62.0,20.,1880.,4*0.0,1.,0.,0.,1.,3.765,0.078,0./
c     *,hypparamet/"_SLL4'_CSB2"/
cccccccccccccccc SLL4* + CSB III ccccccccccccccccccccccccccccccccccccc
c      data tl/-326.0,62.0,20.,1880.,4*0.0,1.,0.,0.,1.,-5.611,3.648,0./
c     *,hypparamet/"_SLL4'_CSB3"/
cccccccccccccccc SLL4* + CSB IV ccccccccccccccccccccccccccccccccccccc
c      data tl/-326.0,62.0,20.,1880.,4*0.0,1.,0.,0.,1.,-6.595,1.463,0./
c     *,hypparamet/"_SLL4'_CSB4"/
cccccccccccccccc SLL4* + CSB V ccccccccccccccccccccccccccccccccccccc
c      data tl/-326.0,62.0,20.,1880.,4*0.0,1.,0.,0.,1.,-6.828,8.151,0./
c     *,hypparamet/"_SLL4'_CSB5"/
cccccccccccccccc SLL4* + CSB VI ccccccccccccccccccccccccccccccccccccc
c      data tl/-326.0,62.0,20.,1880.,4*0.0,1.,0.,0.,1.,1.442,-0.206,0./
c     *,hypparamet/"_SLL4'_CSB6"/
cccccccccccccccc SLL4* + CSB VII ccccccccccccccccccccccccccccccccccccc
c      data tl/-326.0,62.0,20.,1880.,4*0.0,1.,0.,0.,1.,0.520,0.124,0./
c     *,hypparamet/"_SLL4'_CSB7"/
cccccccccccccccc SLL4 no CSB cccccccccccccccccccccccccccccccccccc
c      data tl/-316.0,51.01,41.99,1733.333,4*0.0,1.,0.,0.,1.,0.,0.,0./
c     *,hypparamet/"_SLL4"/
c===========================================================================
c      data bf/2.6,2.4,2.4,2.1,2.1,1.9,1.7,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,1.7,2.,3*0./
c      data bf/2.6,2.4,2.4,2.1,2.1,1.9,
c     *2.6,2.4,2.3,2.1,2.,1.9,1.8,6*0./
c        data bf/1.67,1.67,1.67,1.7,1.7,14*0./
c===========================================================================
cccccccccccccc   Z sigma ? cccccccccccccccccccccccccccccccccccccccc
c      data t/-1983.76,362.25,-104.27,11861.4,1.1717,123.69,.25,
c     *1.762,0.,0.,2*0.,3*0./
cccccccccccccc  T1  ? ccccccccccccccccccccccccccccccccccccccccccc
c		data t/-1794.0,298.,-298.,12812.,.154,110.,.33333,
c     *.089,-.5,-.5,2*0.0/
cccccccccccccc  T6  ? ccccccccccccccccccccccccccccccccccccccccccc
c		data t/-1794.2,294.,-294.,12817.,.392,107.,.33333,
c     *.5,-.5,-.5,2*0.0/
cccccccccccccc  SGII  ccccccccccccccccccccccccccccccccccccccccccc
c                data t/-2645.00,340.00,-41.90,15595.0,.090,105.,.16666,
c     *0.06044,-.0588,1.425,2*0.0,0.0,2*0.,105./,paramet/"_SGII"/
cccccccccccccc SGII+T ccccccccccccccccccccccccccccccccccccccccccc
c                data t/-2645.00,340.00,-41.90,15595.0,.090,105.,.16666,
c     *0.06044,-.0588,1.425,2*0.0,1.0,-180.,120.,105./,paramet/"_SGIIT"/
cccccccccccccc SGII+T2 ccccccccccccccccccccccccccccccccccccccccccc
c                data t/-2645.00,340.00,-41.90,15595.0,.090,105.,.16666,
c     *0.06044,-.0588,1.425,2*0.,1.0,-162.5,4.17,105./,paramet/"_SGIIT2"/
cccccccccccccc SAMi ccccccccccccccccccccccccccccccccccccccccccc
c      data t/-1877.75,475.6,-85.2,10219.6,.320,137,.25614,
c     *0.688,-.532,-.014,2*0.0,1.0,0.,0.,
c     *42/,paramet/"_SAMi"/
cccccccccccccc SAMi+T ccccccccccccccccccccccccccccccccccccccccccc
c      data t/-2199.38,533.036,-88.1692,11293.5,.514710,130.026,.179550,
c     *0.944603,-.531674,-.026340,2*0.0,1.0,-39.8048,66.6505,
c     *101.893/,paramet/"_SAMiT"/
cccccccccccccc  T22  ccccccccccccccccccccccccccccccccccccccccccc
c                data t/-2484.4,484.495,-471.454,13787.,.73012,123.225,
c     *.16666,1.188194,-.442635,-.944655,2*0.0,1.0,-90.63,28.863125/
cccccccccccccc  T22 w/o tensor  cccccccccccccccccccccccccccccccccccc
c                data t/-2484.4,484.495,-471.454,13787.,.073012,123.225,
c     *.16666,1.188194,-.442635,-.944655,2*0.0,0.0,0.,0./
cccccccccccccc  T43  cccccccccccccccccccccccccccccccccccc
c              data t/-2490.28,494.608,-255.534,13847.1,.698702,153.103,
c     *.16666,1.135795,-.781655,-.646302,2*0.0,1.0,-61.45,92.3175/
cccccccccccccc  T45  cccccccccccccccccccccccccccccccccccc
c              data t/-2485.01,492.671,-304.046,13793.3,.727016,168.213,
c     *.16666,1.182969,-.727016,-.755428,2*0.0,1.0,65.37375,104.963/
cccccccccccccc  SLy6   ccccccccccccccccccccccccccccccccccccccccccc
c		data t/-2479.50,462.18,-448.61,13673.0,.825,122.,.16666,
c     *1.355,-.465,-1.,2*0.0,1.0,0.,0./
cccccccccccccc  SLy5   ccccccccccccccccccccccccccccccccccccccccccc
c      data t/-2484.88,483.13,-549.40,13763.0,.778,126.,.16666,
c     *1.267,-.328,-1.,2*0.0,1.0,0.,0.,126./,paramet/"_SLy5"/
cccccccccccccc  SLy5+T  ccccccccccccccccccccccccccccccccccccccccccc
c      data t/-2484.88,483.13,-549.40,13763.0,.778,126.,.16666,
c     *1.267,-.328,-1.,2*0.0,1.0,-170.,100./,paramet/"_SLy5T"/
cccccccccccccc   SLy4    ccccccccccccccccccccccccccccccccccccccccccc
               data t/-2488.91,486.82,-546.39,13777.0,.834,123.,.16666,
     *1.354,-.344,-1.,2*0.0,0.0,2*0.,123./,paramet/"_SLy4"/
cccccccccccccc   SLy4+T  ccccccccccccccccccccccccccccccccccccccccccc
c               data t/-2488.91,486.82,-546.39,13777.0,.834,123.,.16666,
c     *1.354,-.344,-1.,2*0.0,1.,-105.,15.,123./,paramet/"_SLy4T"/
cccccccccccccc SLy4 Chandel ccccccccccccccccccccccccccccccccccccccccccc
c               data t/-2488.91,486.82,-546.39,13777.0,.834,74.84,.16666,
c     *1.354,-.344,-1.,2*0.0,0.0,2*0./
cccccccccccccc   SLy4 (*0.9)   ccccccccccccccccccccccccccccccccccccccccccc
c         data t/-2240.01,438.14,-491.75,12399.3,.834,110.7,.16666,
c     *1.354,-.344,-1.,2*0.0,0.0,2*0./
cccccccccccccc  SLy230a   ccccccccccccccccccccccccccccccccccccccccccc
c		data t/-2490.23,489.53,-566.58,13803.0,1.1318,131.,.16666,
c     *1.9219,-.8426,-1.,2*0.0,3*0.0/,paramet/"_SLy230a"/
cccccccccccccc  SLy230b   ccccccccccccccccccccccccccccccccccccccccccc
c      data t/-2488.91,486.82,-546.39,13777.0,0.834,122.69,.16666,
c     *1.3539,-.3438,-1.,2*0.0,0.0/
cccccccccccccc  MSk7  ? ccccccccccccccccccccccccccccccccccccccccccc
cc		data t/-1828.23,259.4,-292.84,13421.7,.576761,118.807,.333333,
cc     *.785290,-.5,-.5,2*0.0/
cccccccccccccc  Sk p  ? ccccccccccccccccccccccccccccccccccccccccccc
c	data t/-2931.7,320.62,-337.41,18708.97,.29215,100.,.16666,
c     *.18103,.65318,-.53732,2*0.0/
cccccccccccccc   Sk II  ccccccccccccccccccccccccccccccccccccccccccc
c		data t/-1169.9,585.6,-27.1,9331.1,.34,105.,1.,0.,5*0.,
c     *2*0./
cccccccccccccc   Sk III  ccccccccccccccccccccccccccccccccccccccccccc
c		data t/-1128.75,395.,-95.,14000.,.45,120.,1.,1.,4*0.,0.,
c     *2*0./,paramet/"_SIII"/
cccccccccccccc   Sk III, (L*S)*0.95  ccccccccccccccccccccccccccccccccccccccccccc
c		data t/-1128.75,395.,-95.,14000.,.45,114.,1.,1.,4*0.,0.,
c     *2*0./
cccccccccccccc   Sk III, (L*S)*0.90  ccccccccccccccccccccccccccccccccccccccccccc
c		data t/-1128.75,395.,-95.,14000.,.45,108.,1.,1.,4*0.,0.,
c     *2*0./
cccccccccccccc   Sk III, (L*S)*0.80  ccccccccccccccccccccccccccccccccccccccccccc
c		data t/-1128.75,395.,-95.,14000.,.45,96.,1.,1.,4*0.,0.,
c     *2*0./
cccccccccccccc   Sk III, (L*S)*0.60  ccccccccccccccccccccccccccccccccccccccccccc
c		data t/-1128.75,395.,-95.,14000.,.45,72.,1.,1.,4*0.,0.,
c     *2*0./
cccccccccccccc   Sk III, (L*S)*0.40  ccccccccccccccccccccccccccccccccccccccccccc
c		data t/-1128.75,395.,-95.,14000.,.45,48.,1.,1.,4*0.,0.,
c     *2*0./
cccccccccccccc   Sk III, (L*S)*0.30  ccccccccccccccccccccccccccccccccccccccccccc
c		data t/-1128.75,395.,-95.,14000.,.45,36.,1.,1.,4*0.,0.,
c     *2*0./
cccccccccccccc   Sk III, (L*S)*0.26  ccccccccccccccccccccccccccccccccccccccccccc
c		data t/-1128.75,395.,-95.,14000.,.45,31.2,1.,1.,4*0.,0.,
c     *2*0./
cccccccccccccc   Sk III, (L*S)*0.25  ccccccccccccccccccccccccccccccccccccccccccc
c		data t/-1128.75,395.,-95.,14000.,.45,30.,1.,1.,4*0.,0.,
c     *2*0./
cccccccccccccc   Sk III, (L*S)*0.15  ccccccccccccccccccccccccccccccccccccccccccc
c		data t/-1128.75,395.,-95.,14000.,.45,18.,1.,1.,4*0.,0.,
c     *2*0./
cccccccccccccc   Sk III, (L*S)*0.00  ccccccccccccccccccccccccccccccccccccccccccc
c		data t/-1128.75,395.,-95.,14000.,.45,0.,1.,1.,4*0.,0.,
c     *2*0./
cccccccccccccc  Sk M* cccccccccccccccccccccccccccccccccccccccccccc
c      data t/-2645.,410.,-135.,15595.,.09,130.,.166667,0.,4*0.,
c     *0.0,0.,0./,paramet/"_SkM"/
cccccccccccccc  Sk M*, (L*S)*0.5 cccccccccccccccccccccccccccccccccccccccccccc
c      data t/-2645.,410.,-135.,15595.,.09,65.,.166667,0.,4*0.,
c     *0.0,0.,0./
cccccccccccccc  Sk M*, (L*S)*0.0 cccccccccccccccccccccccccccccccccccccccccccc
c      data t/-2645.,410.,-135.,15595.,.09,0.,.166667,0.,4*0.,
c     *0.0,0.,0./
cccccccccccccc  SkS3  ? ccccccccccccccccccccccccccccccccccccccccccc
c		data t/-2014.7,361.0,-29.5,12756.,-.319,94.,.2604,
c     *-.904,.732,4.95,2*0.0/
cccccccccccccc  SK A  ? cccccccccccccccccccccccccccccccccccccccccccc
c      data t/-1602.78,570.88,-67.7,8000.,.02,125.,.3333,-.286,4*0./
cccccccccccccc   Sk2  ? ccccccccccccccccccccccccccccccccccccccccccc
c		    data t/-1169.9,586.6,-27.1,9331.1,.34,105.,1.,1.,4*0./
ccccccccccccccc  Sk  IV  ? cccccccccccccccccccccccccccccccccccccccc
c		data t/-1101.81,271.67,-138.33,17000.,.583,115.,1.,1.,4*0./
ccccccccccccccc  Sk MP   ? cccccccccccccccccccccccccccccccccccccccc
c		data t/-2372.24,503.623,57.2783,12585.3,-.157563,160.,.16666,
c     *-.267933,-.402886,-2.95571,2*0./
ccccccccccccccc  E sigma   ? cccccccccccccccccccccccccccccccccccccccc
c		data t/-1664.05,358.83,-137.22,10931.5,1.077,120.14,.35,
c     *1.6918,4*0./
cccccccccccccccc  RATP   ? cccccccccccccccccccccccccccccccccccccccccc
c		data t/-2160.,513.,121.,11600.,.418,120.,.2,.586,-.36,
c	*-2.29,2*0.0/
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      end