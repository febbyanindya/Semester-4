wil = function()
{
  
  ####################### TAMPILAN AWAL ################################
  
  library(tcltk)
  library(tcltk2)
  jendela=tktoplevel(bg="Gainsboro")
  tkwm.title(jendela,"SELAMAT DATANG")
  
  teks1=tkfont.create(family="times",size=15,weight="bold")
  teks2=tkfont.create(family="sans",size=15)
  teks3=tkfont.create(family="times",size=13,weight="bold")
  teks4=tkfont.create(family="times",size=12)
  
  tekss1=tkfont.create(family="times",size=20,weight="bold")
  
  frame=tkframe(jendela,relief="groove",borderwidth=10,bg="Alice Blue")
  
  tkpack(tklabel(frame,text=" ",bg="Alice Blue"))
  tkpack(tklabel(frame,text="SELAMAT DATANG DI PROGRAM",font=tekss1,bg="Alice Blue"))
  tkpack(tklabel(frame,text="UJI WILCOXON",font=tekss1,bg="Alice Blue"))
  tkpack(tklabel(frame,text=" ",bg="Alice Blue"))
  
  mu = function()
  {
    tkdestroy(jendela)
    library(tcltk)
    hal = tktoplevel(bg="Antique White")
    tktitle(hal) = "IDENTITAS DIRI"
    
    frame1=tkframe(hal,relief="raised",borderwidth=13,bg="Slate Gray")
    frame2=tkframe(hal,relief="raised",borderwidth=13,bg="Alice Blue")
    frame3=tkframe(hal,relief="raised",borderwidth=13,bg="Lavender")
    frame4=tkframe(hal,relief="raised",borderwidth=13,bg="Light Steel Blue")
    frame5=tkframe(hal,relief="raised",borderwidth=13,bg="Gainsboro")
    frame6=tkframe(hal,relief="raised",borderwidth=13,bg="Floral White")
    frame7=tkframe(hal,relief="raised",borderwidth=13,bg="Antique White")
    
    tkpack(tklabel(frame1,text="UJI WILCOXON",font=teks1,bg="Slate Gray"))
    tkpack(frame1,fill="both")
    tkpack(tklabel(frame2,text="KELOMPOK 1 KELAS S1",font=teks1,bg="Alice Blue"))
    tkpack(frame2,fill="both")
    tkpack(tklabel(frame4,text="Dwi Cahyani \t\t  : 081911833004",font=teks1,bg="Light Steel Blue"))
    tkpack(tklabel(frame4,text="Fanni Dwi Pratiwi \t\t  : 081911833017",font=teks1,bg="Light Steel Blue"))
    tkpack(tklabel(frame4,text="Febby Anindya Dwi Lestari \t  : 081911833051",font=teks1,bg="Light Steel Blue"))
    tkpack(tklabel(frame4,text="Zafirah Audi Mastura  \t  : 081911833080",font=teks1,bg="Light Steel Blue"))
    tkpack(frame4,fill="both")
    tkpack(tklabel(frame5,text="Dosen Pembimbing :",font=teks1,bg="Gainsboro"))
    tkpack(tklabel(frame5,text="Dr. Nur Chamidah, S.Si., M.Si.",font=teks1,bg="Gainsboro"))
    tkpack(tklabel(frame5,text="Dr.Toha Saifudin, S.Si., M.Si.",font=teks1,bg="Gainsboro"))
    tkpack(frame5,fill="both")
    tkpack(tklabel(frame3,text="Program Studi S1 Statistika
Fakultas Sains dan Teknologi
Universitas Airlangga
2021",font=teks1,bg="Lavender"))
    tkpack(frame3,fill="both")
  
  home=function()
  {
    tkdestroy(hal)
    require(tcltk)
    covermenu=tktoplevel(bg="Lavender")
    tktitle(covermenu)="MENU UTAMA"
    
    tekss1=tkfont.create(family="times",size=17,weight="bold")
    tekss2=tkfont.create(family="sans",size=13)
    
    frame8=tkframe(covermenu,relief="groove",borderwidth=10,bg="Light Steel Blue")
    frame9=tkframe(covermenu,relief="groove",borderwidth=10,bg="Gainsboro")
    frame10=tkframe(covermenu,relief="groove",borderwidth=10,bg="Light Steel Blue")
    
    tkpack(tklabel(frame8,text=" ",bg="Light Steel Blue"))
    tkpack(tklabel(frame8,text="MENU UTAMA PROGRAM",font=tekss1,bg="Light Steel Blue"))
    tkpack(tklabel(frame8,text="UJI WILCOXON",font=tekss1,bg="Light Steel Blue"))
    tkpack(tklabel(frame8,text=" ",bg="Light Steel Blue"))
    tkpack(frame8,fill="both")
    tkpack(tklabel(frame9,text=" ",bg="Gainsboro"))
    tkpack(tklabel(frame9,text="1. Pilih menu 'UJI WILCOXON' untuk pengujian data",font=tekss2,bg="Gainsboro"))
    tkpack(tklabel(frame9,text="2. Pilih menu 'BANTUAN' untuk melihat bantuan",font=tekss2,bg="Gainsboro"))
    tkpack(tklabel(frame9,text=" ",bg="Gainsboro"))
    tkpack(frame9,fill="both")
    
    
    topmenu = tkmenu(covermenu)
    submenu = tkmenu(covermenu)
    tkconfigure(covermenu, menu=topmenu)
    
    
    menu0 = tkmenu(topmenu, tearoff=FALSE)
    submenu = tkmenu(menu0, tearoff=FALSE)
    menu1 = tkmenu(topmenu, tearoff=FALSE)
    
    
    tutupmenu=function()
    {
      tkdestroy(covermenu)
      tkmessageBox(message="TERIMA KASIH TELAH MENGGUNAKAN PROGRAM UJI WILCOXON INI")
    }
    
    keluarmenu=tkbutton(frame10,text="TUTUP PROGRAM",command=tutupmenu,font=tekss2,bg="Gainsboro")
    tkpack(frame10,fill="both")
    tkpack(keluarmenu,padx=8,pady=8,side="bottom")
    tkpack(frame10,padx=8,pady=8,side="bottom")  
    
    ######## UJI WILCOXON IMPOR DATA #######
    #----- EXCEL -----
    excel = function()
    {
        library(tcltk)
        library(readxl)
      
        tkmessageBox(type="ok", message=" Syarat data yang digunakan:\n
        1. File dalam format Excel (.xlsx)\n
        2. Data dengan 2 sampel berpasangan yang berada pada 2 kolom,\n
           Kolom 1 merupakan data sebelum dan Kolom 2 data sesudah\n
        3. Data dimulai dari baris pertama (Tanpa keterangan kolom)")
      
        data = read_excel(file.choose(), col_names = FALSE)
        
        dataa = as.matrix(data)
        sebelum = as.vector(dataa[,1])
        sesudah = as.vector(dataa[,2]) 
        selisih = as.vector(dataa[,2]-dataa[,1])
        
        a = selisih[selisih!=0]
        b = sort(a) 
        n = tclVar(length(a))
        nn = as.numeric(tclvalue(n))
        rank1 = rank(abs(b))
        
        rankneg1 = rep(0,nn)
        rankpos1 = rep(0,nn)
        
        for(i in 1:nn)
        {
          if(b[i]<0) {rankneg1[i]=rankneg1[i]+rank1[i]}
          else if(b[i]>0) {rankpos1[i]=rankpos1[i]+rank1[i]}
        }
        rmin1 = sum(rankneg1)
        rplus1 = sum(rankpos1)
        w1 = rmin1+rplus1
        t1 = min(rmin1,rplus1)
        
        #-------- UJI NORMALITAS-------------
        normal = ks.test(sebelum,sesudah)
        normaltes = as.numeric(round(normal[[2]],4))
        
        norm1 = tktoplevel(bg="Light Steel Blue")
        tktitle(norm1) = "UJI NORMALITAS"
        normaltes1 = tklabel(norm1,text=as.numeric(normaltes))
        scr = tkscrollbar(norm1,orient="vertical",command=function(...)tkyview(teks1,...))
        teks0 = tktext(norm1, bg="Light Steel Blue",width=85,height=20,yscrollcommand=function(...)tkset(scr,...))
        tkgrid(teks0,scr,sticky="news")
        tkgrid.rowconfigure(norm1,teks0,weight=1)
        tkgrid.columnconfigure(norm1,teks0,weight=1)
        tkinsert(teks0,"end",paste("
        =========================================
         HASIL UJI NORMALITAS KOLMOGOROV-SMIRNOV
        =========================================
	
        HIPOTESIS\t:
        H0\t: Data berdistribusi normal
        H1\t: Data tidak berdistribusi normal
		
        DAERAH KRITIS\t:
        Tolak Ho Jika
        P-Value < 0.05
	
        STATISTIK UJI\t:
        P-Value  = ",normaltes))
        
        if(normaltes < 0.05)
        {
          tkinsert(teks0,"end",paste("
	
        KEPUTUSAN\t:
        Tolak Ho
        Karena\t",normaltes,"< 0.05
    
        KESIMPULAN\t: Data tidak berdistribusi normal"))
          
          #----- Pilihan Uji -----
          mainwil = function()
          {
            tkdestroy(norm1)
            library(tcltk)
            impor = tktoplevel(bg="Light Steel Blue")
            tktitle(impor) = "UJI WILCOXON - IMPOR DATA"
            
            ns1 = tclVar("(Sampel 1)")
            e2 = tkentry(impor, width=20, textvariable=ns1)
            tkgrid(tklabel(impor, text="Nama Sampel 1 :", font=teks3, bg="Light Steel Blue"),sticky="e")
            tkgrid(e2,sticky="e")
            ns2 = tclVar("(Sampel 2)")
            e3 = tkentry(impor, width=20, textvariable=ns2)
            tkgrid(tklabel(impor, text="Nama Sampel 2 :", font=teks3, bg="Light Steel Blue"),sticky="e")
            tkgrid(e3,sticky="e")
            alpha = tclVar("0.05")
            e4 = tkentry(impor, width=20, textvariable=alpha)
            tkgrid(tklabel(impor, text="Alpha :", font=teks3, bg="Light Steel Blue"),sticky="e")
            tkgrid(e4,sticky="e")
            tkgrid(tklabel(impor, text=" ", bg="Light Steel Blue"))
            
            tkgrid(tklabel(impor, text="Hipotesis Alternatif :", font=teks3, bg="Light Steel Blue"),sticky="e")
            rb1 = tkradiobutton(impor)
            rb2 = tkradiobutton(impor)
            rb3 = tkradiobutton(impor)
            
            alter = tclVar("kanan")
            
            tkconfigure(rb1,variable=alter,value="kanan", bg="Light Steel Blue")
            tkconfigure(rb2,variable=alter,value="kiri", bg="Light Steel Blue")
            tkconfigure(rb3,variable=alter,value="duasisi", bg="Light Steel Blue")
            tkgrid(tklabel(impor,text="Lebih besar", font=teks4, bg="Light Steel Blue"),rb1)
            tkgrid(tklabel(impor,text="Lebih kecil", font=teks4, bg="Light Steel Blue"),rb2)
            tkgrid(tklabel(impor,text="Tidak sama dengan", font=teks4, bg="Light Steel Blue"),rb3)
            
            tkgrid(tklabel(impor, text=" ", bg="Light Steel Blue"))
            
            #-------IMPOR KANAN--------
            imporkanan = function()
            {
              if(tclvalue(ns1)=="(Sampel 1)"||tclvalue(ns2)=="(Sampel 2)"||tclvalue(alpha)==" "||tclvalue(alter)==" ")
              {
                tkmessageBox(type="ok",message="Inputan Belum Lengkap",icon="warning")
              }
              #--- n<30 ----
              else if(tclvalue(n)<30)
              {
                ns11 = tclvalue(ns1)
                ns21 = tclvalue(ns2)
                kak30 = tktoplevel(bg="Light Steel Blue")
                tktitle(kak30) = "Uji Wilcoxon Satu Arah Sisi Kanan"
                nnn1 = as.numeric(tclvalue(n))
                whit1 = w1
                alphaa1 = as.numeric(tclvalue(alpha))
                wtab1 = qsignrank(alphaa1,nnn1) #daerah kritis
                scr=tkscrollbar(kak30,orient="vertical",command=function(...)tkyview(teks1,...))
                teks2=tktext(kak30, bg="Light Steel Blue",width=85,height=20,yscrollcommand=function(...)tkset(scr,...))
                tkgrid(teks2,scr,sticky="news")
                tkgrid.rowconfigure(kak30,teks2,weight=1)
                tkgrid.columnconfigure(kak30,teks2,weight=1)
                tkinsert(teks2,"end",paste("
                =========================================
                 HASIL UJI WILCOXON SATU ARAH SISI KANAN
                =========================================
	
                HIPOTESIS\t:
                H0\t: Tidak terdapat perbedaan antara Median ",ns11," dan Median",ns21,"
                H1\t: Median ",ns11," lebih besar dari Median",ns21,"
		
                DAERAH KRITIS\t:
                Tolak Ho Jika
                W hitung >= W Alpha
                W hitung >= ",wtab1,"		   
	
                STATISTIK UJI\t:
                W hitung = ",whit1))
                
                if(whit1>=wtab1)
                {
                  tkinsert(teks2,"end",paste("
	
                KEPUTUSAN\t:
                Tolak Ho
                Karena\t",whit1,">=",wtab1,
                                             "
    
                KESIMPULAN\t: Median ",ns11," lebih besar dari Median ",ns21))
                }
                
                else
                {
                  tkinsert(teks2,"end",paste("
		
                KEPUTUSAN\t:
                Terima Ho
                Karena\t",whit1,"<",wtab1,
                                             "
    
                KESIMPULAN\t: Tidak terdapat perbedaan antara Median ",ns11," dan Median",ns21))
                }
                tombol2 <- tkbutton(kak30, text="TUTUP", command=function()tkdestroy(kak30))
                tkgrid(tombol2)
              }
              #--- n>=30 ----
              else if(tclvalue(n)>=30)
              {
                ns11.1 = tclvalue(ns1)
                ns21.1 = tclvalue(ns2)
                kal30 = tktoplevel(bg="Light Steel Blue")
                tktitle(kal30) = "Uji Wilcoxon Satu Arah Sisi Kanan"
                nnn1.1 = as.numeric(tclvalue(n))
                t1.1 = t1
                alphaa1.1 = as.numeric(tclvalue(alpha))
                zhit1   = round(((t1.1-((nnn1.1*(nnn1.1+1))/4))/sqrt((nnn1.1*(nnn1.1+1)*(2*nnn1.1+1))/24)),4)
                z1      = tklabel(kal30,text=as.numeric(zhit1))
                dk1  = round(qnorm(1-alphaa1.1),4) #daerah kritis z
                dk11 = tklabel(kal30,text=as.numeric(dk1))
                pvalue1 = round((1-(pnorm(zhit1))),4)
                scr=tkscrollbar(kal30,orient="vertical",command=function(...)tkyview(teks1,...))
                teks3=tktext(kal30, bg="Light Steel Blue",width=85,height=20,yscrollcommand=function(...)tkset(scr,...))
                tkgrid(teks3,scr,sticky="news")
                tkgrid.rowconfigure(kal30,teks3,weight=1)
                tkgrid.columnconfigure(kal30,teks3,weight=1)
                tkinsert(teks3,"end",paste("
                =========================================
                 HASIL UJI WILCOXON SATU ARAH SISI KANAN
                =========================================
	
                HIPOTESIS\t:
                H0\t: Tidak terdapat perbedaan antara Median ",ns11," dan Median",ns21,"
                H1\t: Median ",ns11.1," lebih besar dari Median",ns21.1,"
		
                DAERAH KRITIS\t:
                Tolak Ho Jika
                1. Z hitung > Z alpha
                   Z hitung > ",dk1,"		   
                2. P-Value < Alpha
                   P-Value < ",alphaa1.1,"
  	
                STATISTIK UJI\t:
                Z hitung = ",zhit1,"
                P-Value  = ",pvalue1))
                
                if(zhit1>dk1)
                {
                  tkinsert(teks3,"end",paste("
	
                KEPUTUSAN\t:
                Tolak Ho
                Karena\t1.",zhit1,">",dk1,"
                \t   2.",pvalue1,"<",alphaa1.1,
                                             "
    
                KESIMPULAN\t: Median ",ns11.1," lebih besar dari Median",ns21.1))
                }
                
                else
                {
                  tkinsert(teks3,"end",paste("
		
                KEPUTUSAN\t:
                Terima Ho
                Karena\t1.",zhit1,"<",dk1,"
                \t   2.",pvalue1,">",alphaa1.1,
                                             "
    
                KESIMPULAN\t: Tidak terdapat perbedaan antara Median ",ns11," dan Median",ns21))
                }
                tombol3 = tkbutton(kal30, text="TUTUP", command=function()tkdestroy(kal30))
                tkgrid(tombol3)
              }
            }
            
            #-------IMPOR KIRI--------
            imporkiri = function()
            {
              if(tclvalue(ns1)=="(Sampel 1)"||tclvalue(ns2)=="(Sampel 2)"||tclvalue(alpha)==" "||tclvalue(alter)==" ")
              {
                tkmessageBox(type="ok",message="Inputan Belum Lengkap",icon="warning")
              }
              #--- n<30 ----
              else if(tclvalue(n)<30)
              {
                ns12 = tclvalue(ns1)
                ns22 = tclvalue(ns2)
                kik30 = tktoplevel(bg="Light Steel Blue")
                tktitle(kik30) = "Uji Wilcoxon Satu Arah Sisi Kiri"
                nnn2 = as.numeric(tclvalue(n))
                whit2 = w1
                alphaa2 = as.numeric(tclvalue(alpha))
                wtab2 = nnn2*(2*nnn2+1)-qsignrank(alphaa2,nnn2) #daerah kritis
                scr=tkscrollbar(kik30,orient="vertical",command=function(...)tkyview(teks1,...))
                teks2=tktext(kik30, bg="Light Steel Blue",width=85,height=20,yscrollcommand=function(...)tkset(scr,...))
                tkgrid(teks2,scr,sticky="news")
                tkgrid.rowconfigure(kik30,teks2,weight=1)
                tkgrid.columnconfigure(kik30,teks2,weight=1)
                tkinsert(teks2,"end",paste("
                =========================================
                 HASIL UJI WILCOXON SATU ARAH SISI KIRI
                =========================================
	
                HIPOTESIS\t:
                H0\t: Tidak terdapat perbedaan antara Median ",ns12," dan Median",ns22,"
                H1\t: Median ",ns12," lebih kecil dari Median",ns22,"
		
                DAERAH KRITIS\t:
                Tolak Ho Jika
                W hitung <= n*(2*n+1) - W Alpha
                W hitung <= ",wtab2,"		   
	
                STATISTIK UJI\t:
                W hitung = ",whit2))
                
                if(whit2<=wtab2)
                {
                  tkinsert(teks2,"end",paste("
	
                KEPUTUSAN\t:
                Tolak Ho
                Karena\t",whit2,"<=",wtab2,
                                             "
    
                KESIMPULAN\t: Median ",ns12," lebih kecil dari Median",ns22))
                }
                
                else
                {
                  tkinsert(teks2,"end",paste("
		
                KEPUTUSAN\t:
                Terima Ho
                Karena\t",whit2,"<",wtab2,
                                             "
    
                KESIMPULAN\t: Tidak terdapat perbedaan antara Median ",ns12," dan Median",ns22))
                }
                tombol4 <- tkbutton(kik30, text="TUTUP", command=function()tkdestroy(kik30))
                tkgrid(tombol4)
              }
              #--- n>=30 ----
              else if(tclvalue(n)>=30)
              {
                ns12.1 = tclvalue(ns1)
                ns22.1 = tclvalue(ns2)
                kil30 = tktoplevel(bg="Light Steel Blue")
                tktitle(kil30) = "Uji Wilcoxon Satu Arah Sisi Kiri"
                nnn2.1 = as.numeric(tclvalue(n))
                t1.2 = t1
                alphaa2.1 = as.numeric(tclvalue(alpha))
                zhit2   = round(((t1.2-((nnn2.1*(nnn2.1+1))/4))/sqrt((nnn2.1*(nnn2+1)*(2*nnn2.1+1))/24)),4)
                z2      = tklabel(kil30,text=as.numeric(zhit2))
                dk2  = round(qnorm(1-alphaa2.1),4) #daerah kritis
                dk22 = tklabel(kil30,text=as.numeric(dk2))
                pvalue2 = round((pnorm(zhit2)),4)
                scr=tkscrollbar(kil30,orient="vertical",command=function(...)tkyview(teks1,...))
                teks3=tktext(kil30, bg="Light Steel Blue",width=85,height=20,yscrollcommand=function(...)tkset(scr,...))
                tkgrid(teks3,scr,sticky="news")
                tkgrid.rowconfigure(kil30,teks3,weight=1)
                tkgrid.columnconfigure(kil30,teks3,weight=1)
                tkinsert(teks3,"end",paste("
                =========================================
                  HASIL UJI WILCOXON SATU ARAH SISI KIRI
                =========================================
    
                HIPOTESIS\t:
                H0\t: Tidak terdapat perbedaan antara Median ",ns12.1," dan Median",ns22.1,"
                H1\t: Median ",ns12.1," lebih kecil dari Median",ns22.1,"
		
                DAERAH KRITIS\t:
                Tolak Ho Jika
                1. Z hitung < - Z alpha
                   Z hitung < ",(-dk2),"		   
                2. P-Value < Alpha
                   P-Value < ",alphaa2.1,"
	
                STATISTIK UJI\t:
                Z hitung = ",zhit2,"
                P-Value  = ",pvalue2))
                
                if(zhit2<(-dk2))
                {
                  tkinsert(teks3,"end",paste("
	
                KEPUTUSAN\t:
                Tolak Ho
                Karena\t1.",zhit2,"<",(-dk2),"
                \t   2.",pvalue2,"<",alphaa2.1,
                                             "
    
                KESIMPULAN\t: Median ",ns12.1," lebih kecil dari Median",ns22.1))
                }
                
                else
                {
                  tkinsert(teks3,"end",paste("
		
                KEPUTUSAN\t:
                Terima Ho
                Karena\t1.",zhit2,">",(-dk2),"
                \t   2.",pvalue2,">",alphaa2.1,
                                             "
    
                KESIMPULAN\t: Tidak terdapat perbedaan antara Median ",ns12.1," dan Median",ns22.1))
                }
                tombol5 <- tkbutton(kil30, text="TUTUP", command=function()tkdestroy(kil30))
                tkgrid(tombol5)
              }
            }
            
            #-------IMPOR DUA SISI--------
            imporduasisi = function()
            {
              if(tclvalue(ns1)=="(Sampel 1)"||tclvalue(ns2)=="(Sampel 2)"||tclvalue(alpha)==" "||tclvalue(alter)==" ")
              {
                tkmessageBox(type="ok",message="Inputan Belum Lengkap",icon="warning")
              }
              #--- n<30 ----
              else if(tclvalue(n)<30)
              {
                ns13 = tclvalue(ns1)
                ns23 = tclvalue(ns2)
                dsk30 = tktoplevel()
                tktitle(dsk30) = "Uji Wilcoxon Dua Arah"
                nnn3 = as.numeric(tclvalue(n))
                whit3 = w1
                alphaa3 = as.numeric(tclvalue(alpha))
                wtab31 = qsignrank(alphaa3/2,nnn3)
                wtab32 = nnn3*(2*nnn3+1)-wtab31
                
                scr=tkscrollbar(dsk30,orient="vertical",command=function(...)tkyview(teks1,...))
                teks2=tktext(dsk30, bg="Light Steel Blue",width=85,height=20,yscrollcommand=function(...)tkset(scr,...))
                tkgrid(teks2,scr,sticky="news")
                tkgrid.rowconfigure(dsk30,teks2,weight=1)
                tkgrid.columnconfigure(dsk30,teks2,weight=1)
                tkinsert(teks2,"end",paste("
	              =========================================
	                    HASIL UJI WILCOXON DUA ARAH
	              =========================================
	
	              HIPOTESIS\t:
	              H0\t: Tidak terdapat perbedaan antara Median ",ns13," dan Median",ns23,"
	              H1\t: Terdapat perbedaan antara Median ",ns13,"dan Median",ns23,"
	  
	              DAERAH KRITIS\t:
	              Tolak Ho Jika
	              W hitung >= W alpha/2
	              W hitung >= ",wtab31,"
	              atau\n
	              W hitung <= n*(2*n+1) - W alpha/2
	              W hitung <= ",wtab32,"
	      
	              STATISTIK UJI\t:
                \t     W hitung = ",whit3))
                
                if(whit3>=wtab31||whit3<=wtab32)
                {
                  tkinsert(teks2,"end",paste("
	
	              KEPUTUSAN\t:
	              Tolak Ho
	              Karena\t",whit3,">=",wtab31,"
	              atau",whit3,"<=",wtab32,"
        
                \t     KESIMPULAN\t: Terdapat perbedaan antara Median ",ns13,"dan Median",ns23))
                }
                
                else
                {
                  tkinsert(teks2,"end",paste("
		
	              KEPUTUSAN\t:
	              Terima Ho
	              Karena\t",wtab32,"<=",whit3,"<=",wtab32,
                                             "
    
                \t  KESIMPULAN\t: Tidak terdapat perbedaan antara Median ",ns13," dan Median",ns23))
                }
                tombol6 <- tkbutton(dsk30, text="TUTUP", command=function()tkdestroy(dsk30))
                tkgrid(tombol6)
              }
              #--- n>=30 ----
              else if(tclvalue(n)>=30)
              {
                ns13.1 = tclvalue(ns1)
                ns23.1 = tclvalue(ns2)
                dsl30 = tktoplevel(bg="Light Steel Blue")
                tktitle(dsl30) = "Uji Wilcoxon Dua Arah"
                nnn3.1 = as.numeric(tclvalue(n))
                t1.3 = t1
                alphaa3.1 = as.numeric(tclvalue(alpha))
                zhit3   = round(((t1.3-((nnn3.1*(nnn3.1+1))/4))/sqrt((nnn3.1*(nnn2+1)*(2*nnn3.1+1))/24)),4)
                z3      = tklabel(dsl30,text=as.numeric(zhit3))
                dk3  = round(qnorm(1-alphaa3.1/2),4) #daerah kritis
                dk33 = tklabel(dsl30,text=as.numeric(dk3))
                palue3 = round((2*(1-pnorm(abs(zhit3)))),4)
                scr=tkscrollbar(dsl30,orient="vertical",command=function(...)tkyview(teks1,...))
                teks3=tktext(dsl30, bg="Light Steel Blue",width=85,height=20,yscrollcommand=function(...)tkset(scr,...))
                tkgrid(teks3,scr,sticky="news")
                tkgrid.rowconfigure(dsl30,teks3,weight=1)
                tkgrid.columnconfigure(dsl30,teks3,weight=1)
                tkinsert(teks3,"end",paste("
                =========================================
                       HASIL UJI WILCOXON DUA ARAH
                =========================================
    
                HIPOTESIS\t:
                H0\t: Tidak terdapat perbedaan antara Median ",ns13.1," dan Median",ns23.1,"
                H1\t: Terdapat perbedaan antara Median ",ns13.1,"dan Median",ns23.1,"
		
                DAERAH KRITIS\t:
                Tolak Ho Jika
                1. |Z hitung| >  Z alpha
                   |Z hitung| > ",dk3,"		   
                2. P-Value < Alpha
                   P-Value < ",alphaa3.1,"
	
                STATISTIK UJI\t:
                Z hitung = ",zhit3,"
                P-Value  = ",palue3))
                
                if(abs(zhit3)>dk3)
                {
                  tkinsert(teks3,"end",paste("
	
                KEPUTUSAN\t:
                Tolak Ho
                Karena\t1.|",zhit3,"| >",dk3,"
                \t   2.",palue3,"<",alphaa3.1,
                                             "
    
                KESIMPULAN\t: Terdapat perbedaan antara Median ",ns13.1," dan Median",ns23.1))
                }
                
                else
                {
                  tkinsert(teks3,"end",paste("
		
                KEPUTUSAN\t:
                Terima Ho
                Karena\t1.|",zhit3,"| <",dk3,"
                \t   2.",palue3,">",alphaa3.1,
                                             "
    
                KESIMPULAN\t: Tidak terdapat perbedaan antara Median ",ns13.1," dan Median",ns23.1))
                }
                tombol7 <- tkbutton(dsl30, text="TUTUP", command=function()tkdestroy(dsl30))
                tkgrid(tombol7)
              }
            }
            
            #-------OK-----------
            ok = function()
            {
              altern = as.character(tclvalue(alter))
              if(altern=="kanan")
                imporkanan()
              else if(altern=="kiri")
                imporkiri()
              else if(altern=="duasisi")
                imporduasisi()
            }
            tombol0 = tkbutton(impor, text="UJI WILCOXON", command=ok)
            tkgrid(tombol0)
            tombol = tkbutton(impor, text="KEMBALI", command=function()tkdestroy(impor))
            tkgrid(tombol)
          }
          tombol1 = tkbutton(norm1, text="LANJUTKAN", command=mainwil)
          tkgrid(tombol1)
        }
        
        else
        {
          tkinsert(teks0,"end",paste("
		
    KEPUTUSAN\t:
    Terima Ho
    Karena\t",normaltes,"> 0.05
    
    KESIMPULAN\t: Data berdistribusi normal"))
          
          tutup = function()
          {
            tkmessageBox(type="ok",message="Maaf, data tidak cocok untuk UJI WILCOXON\ngunakan data lainnya",icon="warning")
            tkdestroy(norm1)
          }
          tombol2 = tkbutton(norm1, text="LANJUTKAN", command=tutup)
          tkgrid(tombol2)  
        }
      }
    #----- SPSS -----
    spss = function()
    {
        library(tcltk)
        library(foreign)
        
        tkmessageBox(type="ok", message=" Syarat data yang digunakan:\n
        1. File dalam format SPSS (.sav)\n
        2. Data dengan 2 sampel berpasangan yang berada pada 2 kolom,\n
           Kolom 1 merupakan data sebelum dan Kolom 2 data sesudah")
        
        data = read.spss(file.choose(), use.value.labels=TRUE,max.value.labels=inf,to.data.frame=TRUE)
        
        dataa = as.matrix(data)
        sebelum = as.vector(dataa[,1])
        sesudah = as.vector(dataa[,2]) 
        selisih = as.vector(dataa[,2]-dataa[,1])
        
        a = selisih[selisih!=0]
        b = sort(a) 
        n = tclVar(length(a))
        nn = as.numeric(tclvalue(n))
        rank1 = rank(abs(b))
        
        rankneg1 = rep(0,nn)
        rankpos1 = rep(0,nn)
        
        for(i in 1:nn)
        {
          if(b[i]<0) {rankneg1[i]=rankneg1[i]+rank1[i]}
          else if(b[i]>0) {rankpos1[i]=rankpos1[i]+rank1[i]}
        }
        rmin1 = sum(rankneg1)
        rplus1 = sum(rankpos1)
        w1 = rmin1+rplus1
        t1 = min(rmin1,rplus1)
        
        #-------- UJI NORMALITAS-------------
        normal = ks.test(sebelum,sesudah)
        normaltes = as.numeric(round(normal[[2]],4))
        
        norm1 = tktoplevel(bg="Light Steel Blue")
        tktitle(norm1) = "UJI NORMALITAS"
        normaltes1 = tklabel(norm1,text=as.numeric(normaltes))
        scr = tkscrollbar(norm1,orient="vertical",command=function(...)tkyview(teks1,...))
        teks0 = tktext(norm1, bg="Light Steel Blue",width=85,height=20,yscrollcommand=function(...)tkset(scr,...))
        tkgrid(teks0,scr,sticky="news")
        tkgrid.rowconfigure(norm1,teks0,weight=1)
        tkgrid.columnconfigure(norm1,teks0,weight=1)
        tkinsert(teks0,"end",paste("
        =========================================
         HASIL UJI NORMALITAS KOLMOGOROV-SMIRNOV
        =========================================
	
        HIPOTESIS\t:
        H0\t: Data berdistribusi normal
        H1\t: Data tidak berdistribusi normal
		
        DAERAH KRITIS\t:
        Tolak Ho Jika
        P-Value < 0.05
	
        STATISTIK UJI\t:
        P-Value  = ",normaltes))
        
        if(normaltes < 0.05)
        {
          tkinsert(teks0,"end",paste("
	
        KEPUTUSAN\t:
        Tolak Ho
        Karena\t",normaltes,"< 0.05
    
        KESIMPULAN\t: Data tidak berdistribusi normal"))
          
          #----- Pilihan Uji -----
          mainwil = function()
          {
            tkdestroy(norm1)
            library(tcltk)
            impor = tktoplevel(bg="Light Steel Blue")
            tktitle(impor) = "UJI WILCOXON - IMPOR DATA"
            
            ns1 = tclVar("(Sampel 1)")
            e2 = tkentry(impor, width=20, textvariable=ns1)
            tkgrid(tklabel(impor, text="Nama Sampel 1 :", font=teks3, bg="Light Steel Blue"),sticky="e")
            tkgrid(e2,sticky="e")
            ns2 = tclVar("(Sampel 2)")
            e3 = tkentry(impor, width=20, textvariable=ns2)
            tkgrid(tklabel(impor, text="Nama Sampel 2 :", font=teks3, bg="Light Steel Blue"),sticky="e")
            tkgrid(e3,sticky="e")
            alpha = tclVar("0.05")
            e4 = tkentry(impor, width=20, textvariable=alpha)
            tkgrid(tklabel(impor, text="Alpha :", font=teks3, bg="Light Steel Blue"),sticky="e")
            tkgrid(e4,sticky="e")
            tkgrid(tklabel(impor, text=" ", bg="Light Steel Blue"))
            
            tkgrid(tklabel(impor, text="Hipotesis Alternatif :", font=teks3, bg="Light Steel Blue"),sticky="e")
            rb1 = tkradiobutton(impor)
            rb2 = tkradiobutton(impor)
            rb3 = tkradiobutton(impor)
            
            alter = tclVar("kanan")
            
            tkconfigure(rb1,variable=alter,value="kanan", bg="Light Steel Blue",anchor="w")
            tkconfigure(rb2,variable=alter,value="kiri", bg="Light Steel Blue",anchor="w")
            tkconfigure(rb3,variable=alter,value="duasisi", bg="Light Steel Blue",anchor="w")
            tkgrid(tklabel(impor,text="Lebih besar", font=teks4, bg="Light Steel Blue"),rb1)
            tkgrid(tklabel(impor,text="Lebih kecil", font=teks4, bg="Light Steel Blue"),rb2)
            tkgrid(tklabel(impor,text="Tidak sama dengan", font=teks4, bg="Light Steel Blue"),rb3)
            
            tkgrid(tklabel(impor, text=" ", bg="Light Steel Blue"))
            
            #-------IMPOR KANAN--------
            imporkanan = function()
            {
              if(tclvalue(ns1)=="(Sampel 1)"||tclvalue(ns2)=="(Sampel 2)"||tclvalue(alpha)==" "||tclvalue(alter)==" ")
              {
                tkmessageBox(type="ok",message="Inputan Belum Lengkap",icon="warning")
              }
              #--- n<30 ----
              else if(tclvalue(n)<30)
              {
                ns11 = tclvalue(ns1)
                ns21 = tclvalue(ns2)
                kak30 = tktoplevel(bg="Light Steel Blue")
                tktitle(kak30) = "Uji Wilcoxon Satu Arah Sisi Kanan"
                nnn1 = as.numeric(tclvalue(n))
                whit1 = w1
                alphaa1 = as.numeric(tclvalue(alpha))
                wtab1 = qsignrank(alphaa1,nnn1) #daerah kritis
                scr=tkscrollbar(kak30,orient="vertical",command=function(...)tkyview(teks1,...))
                teks2=tktext(kak30, bg="Light Steel Blue",width=85,height=20,yscrollcommand=function(...)tkset(scr,...))
                tkgrid(teks2,scr,sticky="news")
                tkgrid.rowconfigure(kak30,teks2,weight=1)
                tkgrid.columnconfigure(kak30,teks2,weight=1)
                tkinsert(teks2,"end",paste("
                =========================================
                 HASIL UJI WILCOXON SATU ARAH SISI KANAN
                =========================================
	
                HIPOTESIS\t:
                H0\t: Tidak terdapat perbedaan antara Median ",ns11," dan Median",ns21,"
                H1\t: Median ",ns11," lebih besar dari Median",ns21,"
		
                DAERAH KRITIS\t:
                Tolak Ho Jika
                W hitung >= W Alpha
                W hitung >= ",wtab1,"		   
	
                STATISTIK UJI\t:
                W hitung = ",whit1))
                
                if(whit1>=wtab1)
                {
                  tkinsert(teks2,"end",paste("
	
                KEPUTUSAN\t:
                Tolak Ho
                Karena\t",whit1,">=",wtab1,
                                             "
    
                KESIMPULAN\t: Median ",ns11," lebih besar dari Median ",ns21))
                }
                
                else
                {
                  tkinsert(teks2,"end",paste("
		
                KEPUTUSAN\t:
                Terima Ho
                Karena\t",whit1,"<",wtab1,
                                             "
    
                KESIMPULAN\t: Tidak terdapat perbedaan antara Median ",ns11," dan Median",ns21))
                }
                tombol2 <- tkbutton(kak30, text="TUTUP", command=function()tkdestroy(kak30))
                tkgrid(tombol2)
              }
              #--- n>=30 ----
              else if(tclvalue(n)>=30)
              {
                ns11.1 = tclvalue(ns1)
                ns21.1 = tclvalue(ns2)
                kal30 = tktoplevel(bg="Light Steel Blue")
                tktitle(kal30) = "Uji Wilcoxon Satu Arah Sisi Kanan"
                nnn1.1 = as.numeric(tclvalue(n))
                t1.1 = t1
                alphaa1.1 = as.numeric(tclvalue(alpha))
                zhit1   = round(((t1.1-((nnn1.1*(nnn1.1+1))/4))/sqrt((nnn1.1*(nnn1.1+1)*(2*nnn1.1+1))/24)),4)
                z1      = tklabel(kal30,text=as.numeric(zhit1))
                dk1  = round(qnorm(1-alphaa1.1),4) #daerah kritis z
                dk11 = tklabel(kal30,text=as.numeric(dk1))
                pvalue1 = round((1-(pnorm(zhit1))),4)
                scr=tkscrollbar(kal30,orient="vertical",command=function(...)tkyview(teks1,...))
                teks3=tktext(kal30, bg="Light Steel Blue",width=85,height=20,yscrollcommand=function(...)tkset(scr,...))
                tkgrid(teks3,scr,sticky="news")
                tkgrid.rowconfigure(kal30,teks3,weight=1)
                tkgrid.columnconfigure(kal30,teks3,weight=1)
                tkinsert(teks3,"end",paste("
                =========================================
                 HASIL UJI WILCOXON SATU ARAH SISI KANAN
                =========================================
	
                HIPOTESIS\t:
                H0\t: Tidak terdapat perbedaan antara Median ",ns11," dan Median",ns21,"
                H1\t: Median ",ns11.1," lebih besar dari Median",ns21.1,"
		
                DAERAH KRITIS\t:
                Tolak Ho Jika
                1. Z hitung > Z alpha
                   Z hitung > ",dk1,"		   
                2. P-Value < Alpha
                   P-Value < ",alphaa1.1,"
  	
                STATISTIK UJI\t:
                Z hitung = ",zhit1,"
                P-Value  = ",pvalue1))
                
                if(zhit1>dk1)
                {
                  tkinsert(teks3,"end",paste("
	
                KEPUTUSAN\t:
                Tolak Ho
                Karena\t1.",zhit1,">",dk1,"
                \t   2.",pvalue1,"<",alphaa1.1,
                                             "
    
                KESIMPULAN\t: Median ",ns11.1," lebih besar dari Median",ns21.1))
                }
                
                else
                {
                  tkinsert(teks3,"end",paste("
		
                KEPUTUSAN\t:
                Terima Ho
                Karena\t1.",zhit1,"<",dk1,"
                \t   2.",pvalue1,">",alphaa1.1,
                                             "
    
                KESIMPULAN\t: Tidak terdapat perbedaan antara Median ",ns11," dan Median",ns21))
                }
                tombol3 = tkbutton(kal30, text="TUTUP", command=function()tkdestroy(kal30))
                tkgrid(tombol3)
              }
            }
            
            #-------IMPOR KIRI--------
            imporkiri = function()
            {
              if(tclvalue(ns1)=="(Sampel 1)"||tclvalue(ns2)=="(Sampel 2)"||tclvalue(alpha)==" "||tclvalue(alter)==" ")
              {
                tkmessageBox(type="ok",message="Inputan Belum Lengkap",icon="warning")
              }
              #--- n<30 ----
              else if(tclvalue(n)<30)
              {
                ns12 = tclvalue(ns1)
                ns22 = tclvalue(ns2)
                kik30 = tktoplevel(bg="Light Steel Blue")
                tktitle(kik30) = "Uji Wilcoxon Satu Arah Sisi Kiri"
                nnn2 = as.numeric(tclvalue(n))
                whit2 = w1
                alphaa2 = as.numeric(tclvalue(alpha))
                wtab2 = nnn2*(2*nnn2+1)-qsignrank(alphaa2,nnn2) #daerah kritis
                scr=tkscrollbar(kik30,orient="vertical",command=function(...)tkyview(teks1,...))
                teks2=tktext(kik30, bg="Light Steel Blue",width=85,height=20,yscrollcommand=function(...)tkset(scr,...))
                tkgrid(teks2,scr,sticky="news")
                tkgrid.rowconfigure(kik30,teks2,weight=1)
                tkgrid.columnconfigure(kik30,teks2,weight=1)
                tkinsert(teks2,"end",paste("
                =========================================
                 HASIL UJI WILCOXON SATU ARAH SISI KIRI
                =========================================
	
                HIPOTESIS\t:
                H0\t: Tidak terdapat perbedaan antara Median ",ns12," dan Median",ns22,"
                H1\t: Median ",ns12," lebih kecil dari Median",ns22,"
		
                DAERAH KRITIS\t:
                Tolak Ho Jika
                W hitung <= n*(2*n+1) - W Alpha
                W hitung <= ",wtab2,"		   
	
                STATISTIK UJI\t:
                W hitung = ",whit2))
                
                if(whit2<=wtab2)
                {
                  tkinsert(teks2,"end",paste("
	
                KEPUTUSAN\t:
                Tolak Ho
                Karena\t",whit2,"<=",wtab2,
                                             "
    
                KESIMPULAN\t: Median ",ns12," lebih kecil dari Median",ns22))
                }
                
                else
                {
                  tkinsert(teks2,"end",paste("
		
                KEPUTUSAN\t:
                Terima Ho
                Karena\t",whit2,"<",wtab2,
                                             "
    
                KESIMPULAN\t: Tidak terdapat perbedaan antara Median ",ns12," dan Median",ns22))
                }
                tombol4 <- tkbutton(kik30, text="TUTUP", command=function()tkdestroy(kik30))
                tkgrid(tombol4)
              }
              #--- n>=30 ----
              else if(tclvalue(n)>=30)
              {
                ns12.1 = tclvalue(ns1)
                ns22.1 = tclvalue(ns2)
                kil30 = tktoplevel(bg="Light Steel Blue")
                tktitle(kil30) = "Uji Wilcoxon Satu Arah Sisi Kiri"
                nnn2.1 = as.numeric(tclvalue(n))
                t1.2 = t1
                alphaa2.1 = as.numeric(tclvalue(alpha))
                zhit2   = round(((t1.2-((nnn2.1*(nnn2.1+1))/4))/sqrt((nnn2.1*(nnn2+1)*(2*nnn2.1+1))/24)),4)
                z2      = tklabel(kil30,text=as.numeric(zhit2))
                dk2  = round(qnorm(1-alphaa2.1),4) #daerah kritis
                dk22 = tklabel(kil30,text=as.numeric(dk2))
                pvalue2 = round((pnorm(zhit2)),4)
                scr=tkscrollbar(kil30,orient="vertical",command=function(...)tkyview(teks1,...))
                teks3=tktext(kil30, bg="Light Steel Blue",width=85,height=20,yscrollcommand=function(...)tkset(scr,...))
                tkgrid(teks3,scr,sticky="news")
                tkgrid.rowconfigure(kil30,teks3,weight=1)
                tkgrid.columnconfigure(kil30,teks3,weight=1)
                tkinsert(teks3,"end",paste("
                =========================================
                  HASIL UJI WILCOXON SATU ARAH SISI KIRI
                =========================================
    
                HIPOTESIS\t:
                H0\t: Tidak terdapat perbedaan antara Median ",ns12.1," dan Median",ns22.1,"
                H1\t: Median ",ns12.1," lebih kecil dari Median",ns22.1,"
		
                DAERAH KRITIS\t:
                Tolak Ho Jika
                1. Z hitung < - Z alpha
                   Z hitung < ",(-dk2),"		   
                2. P-Value < Alpha
                   P-Value < ",alphaa2.1,"
	
                STATISTIK UJI\t:
                Z hitung = ",zhit2,"
                P-Value  = ",pvalue2))
                
                if(zhit2<(-dk2))
                {
                  tkinsert(teks3,"end",paste("
	
                KEPUTUSAN\t:
                Tolak Ho
                Karena\t1.",zhit2,"<",(-dk2),"
                \t   2.",pvalue2,"<",alphaa2.1,
                                             "
    
                KESIMPULAN\t: Median ",ns12.1," lebih kecil dari Median",ns22.1))
                }
                
                else
                {
                  tkinsert(teks3,"end",paste("
		
                KEPUTUSAN\t:
                Terima Ho
                Karena\t1.",zhit2,">",(-dk2),"
                \t   2.",pvalue2,">",alphaa2.1,
                                             "
    
                KESIMPULAN\t: Tidak terdapat perbedaan antara Median ",ns12.1," dan Median",ns22.1))
                }
                tombol5 <- tkbutton(kil30, text="TUTUP", command=function()tkdestroy(kil30))
                tkgrid(tombol5)
              }
            }
            
            #-------IMPOR DUA SISI--------
            imporduasisi = function()
            {
              if(tclvalue(ns1)=="(Sampel 1)"||tclvalue(ns2)=="(Sampel 2)"||tclvalue(alpha)==" "||tclvalue(alter)==" ")
              {
                tkmessageBox(type="ok",message="Inputan Belum Lengkap",icon="warning")
              }
              #--- n<30 ----
              else if(tclvalue(n)<30)
              {
                ns13 = tclvalue(ns1)
                ns23 = tclvalue(ns2)
                dsk30 = tktoplevel()
                tktitle(dsk30) = "Uji Wilcoxon Dua Arah"
                nnn3 = as.numeric(tclvalue(n))
                whit3 = w1
                alphaa3 = as.numeric(tclvalue(alpha))
                wtab31 = qsignrank(alphaa3/2,nnn3)
                wtab32 = nnn3*(2*nnn3+1)-wtab31
                
                scr=tkscrollbar(dsk30,orient="vertical",command=function(...)tkyview(teks1,...))
                teks2=tktext(dsk30, bg="Light Steel Blue",width=85,height=20,yscrollcommand=function(...)tkset(scr,...))
                tkgrid(teks2,scr,sticky="news")
                tkgrid.rowconfigure(dsk30,teks2,weight=1)
                tkgrid.columnconfigure(dsk30,teks2,weight=1)
                tkinsert(teks2,"end",paste("
	              =========================================
	                    HASIL UJI WILCOXON DUA ARAH
	              =========================================
	              
	              HIPOTESIS\t:
	              H0\t: Tidak terdapat perbedaan antara Median ",ns13," dan Median",ns23,"
	              H1\t: Terdapat perbedaan antara Median ",ns13,"dan Median",ns23,"
	  
	              DAERAH KRITIS\t:
	              Tolak Ho Jika
	              W hitung >= W alpha/2
	              W hitung >= ",wtab31,"
	              atau\n
	              W hitung <= n*(2*n+1) - W alpha/2
	              W hitung <= ",wtab32,"
	      
	             STATISTIK UJI\t:
               \t     W hitung = ",whit3))
                
                if(whit3>=wtab31||whit3<=wtab32)
                {
                  tkinsert(teks2,"end",paste("
	
	              KEPUTUSAN\t:
	              Tolak Ho
	              Karena\t",whit3,">=",wtab31,"
	              atau",whit3,"<=",wtab32,"
        
                \t     KESIMPULAN\t: Terdapat perbedaan antara Median ",ns13,"dan Median",ns23))
                }
                
                else
                {
                  tkinsert(teks2,"end",paste("
		
	              KEPUTUSAN\t:
	              Terima Ho
	              Karena\t",wtab32,"<=",whit3,"<=",wtab32,
                                             "
    
                \t  KESIMPULAN\t: Tidak terdapat perbedaan antara Median ",ns13," dan Median",ns23))
                }
                tombol6 <- tkbutton(dsk30, text="TUTUP", command=function()tkdestroy(dsk30))
                tkgrid(tombol6)
              }
              #--- n>=30 ----
              else if(tclvalue(n)>=30)
              {
                ns13.1 = tclvalue(ns1)
                ns23.1 = tclvalue(ns2)
                dsl30 = tktoplevel(bg="Light Steel Blue")
                tktitle(dsl30) = "Uji Wilcoxon Dua Arah"
                nnn3.1 = as.numeric(tclvalue(n))
                t1.3 = t1
                alphaa3.1 = as.numeric(tclvalue(alpha))
                zhit3   = round(((t1.3-((nnn3.1*(nnn3.1+1))/4))/sqrt((nnn3.1*(nnn2+1)*(2*nnn3.1+1))/24)),4)
                z3      = tklabel(dsl30,text=as.numeric(zhit3))
                dk3  = round(qnorm(1-alphaa3.1/2),4) #daerah kritis
                dk33 = tklabel(dsl30,text=as.numeric(dk3))
                palue3 = round((2*(1-pnorm(abs(zhit3)))),4)
                scr=tkscrollbar(dsl30,orient="vertical",command=function(...)tkyview(teks1,...))
                teks3=tktext(dsl30, bg="Light Steel Blue",width=85,height=20,yscrollcommand=function(...)tkset(scr,...))
                tkgrid(teks3,scr,sticky="news")
                tkgrid.rowconfigure(dsl30,teks3,weight=1)
                tkgrid.columnconfigure(dsl30,teks3,weight=1)
                tkinsert(teks3,"end",paste("
                =========================================
                      HASIL UJI WILCOXON DUA ARAH
                =========================================
    
                HIPOTESIS\t:
                H0\t: Tidak terdapat perbedaan antara Median ",ns13.1," dan Median",ns23.1,"
                H1\t: Terdapat perbedaan antara Median ",ns13.1,"dan Median",ns23.1,"
		
                DAERAH KRITIS\t:
                Tolak Ho Jika
                1. |Z hitung| >  Z alpha
                   |Z hitung| > ",dk3,"		   
                2. P-Value < Alpha
                   P-Value < ",alphaa3.1,"
	
                STATISTIK UJI\t:
                Z hitung = ",zhit3,"
                P-Value  = ",palue3))
                
                if(abs(zhit3)>dk3)
                {
                  tkinsert(teks3,"end",paste("
	
                KEPUTUSAN\t:
                Tolak Ho
                Karena\t1.|",zhit3,"| >",dk3,"
                \t   2.",palue3,"<",alphaa3.1,
                                             "
    
                KESIMPULAN\t: Terdapat perbedaan antara Median ",ns13.1," dan Median",ns23.1))
                }
                
                else
                {
                  tkinsert(teks3,"end",paste("
		
                KEPUTUSAN\t:
                Terima Ho
                Karena\t1.|",zhit3,"| <",dk3,"
                \t   2.",palue3,">",alphaa3.1,
                                             "
    
                KESIMPULAN\t: Tidak terdapat perbedaan antara Median ",ns13.1," dan Median",ns23.1))
                }
                tombol7 <- tkbutton(dsl30, text="TUTUP", command=function()tkdestroy(dsl30))
                tkgrid(tombol7)
              }
            }
            
            #-------OK-----------
            ok = function()
            {
              altern = as.character(tclvalue(alter))
              if(altern=="kanan")
                imporkanan()
              else if(altern=="kiri")
                imporkiri()
              else if(altern=="duasisi")
                imporduasisi()
            }
            tombol0 = tkbutton(impor, text="UJI WILCOXON", command=ok)
            tkgrid(tombol0)
            tombol = tkbutton(impor, text="KEMBALI", command=function()tkdestroy(impor))
            tkgrid(tombol)
          }
          tombol1 = tkbutton(norm1, text="LANJUTKAN", command=mainwil)
          tkgrid(tombol1)
        }
        
        else
        {
          tkinsert(teks0,"end",paste("
		
        KEPUTUSAN\t:
        Terima Ho
        Karena\t",normaltes,"> 0.05
    
        KESIMPULAN\t: Data berdistribusi normal"))
          
          tutup = function()
          {
            tkmessageBox(type="ok",message="Maaf, data tidak cocok untuk UJI WILCOXON\ngunakan data lainnya",icon="warning")
            tkdestroy(norm1)
          }
          tombol2 = tkbutton(norm1, text="LANJUTKAN", command=tutup)
          tkgrid(tombol2)  
        }
      }
    
    ######## UJI WILCOXON INPUT DATA #######
    inputwilcoxon = function()
    {
      library(tcltk)
      
      norm2 = tktoplevel(bg="Light Steel Blue")
      tktitle(norm2) = "UJI WILCOXON - INPUT DATA"
      
      n1 = tclVar("")
      e5 = tkentry(norm2, width=30, textvariable=n1)
      tkgrid(tklabel(norm2, text="Banyak Data (n) :", font=teks4, bg="Light Steel Blue"))
      tkgrid(e5)
      
      tkgrid(tklabel(norm2, text=" ", bg="Light Steel Blue"))
      
      ok = function()
      {
        if(tclvalue(n1)<=0)
        {	
          tkmessageBox(message="Inputan Salah!",icon="warning")
        }
        else
        {      
          tkdestroy(norm2)
          tkmessageBox(type="ok", message=" Ketentuan Input Data:
          Dua sampel berpasangan, dengan
          1. Kolom Sampel 1 berisi data sebelum 
          2. Kolom Sampel 2 berisi data sesudah\n")
          norm2.1 = tktoplevel(bg="Light Steel Blue")
          tktitle(norm2.1) = "INPUT DATA"
          nn1 = as.numeric(tclvalue(n1))
          tclRequire("Tktable")
          nrow1=nn1
          kol1=1
          wadah1=matrix(0,nrow1,kol1)
          x1=matrix(0,nrow1,kol1)
          isi1=tclArray()
          for (i in 0:nrow1)
          {
            for (j in 0:kol1)
            {
              isi1[[i,j]]<-wadah1[i,j]
            }
          }
          isi1[[0,0]]="Sampel 1"
          tabel1=tkwidget(norm2.1,"table",variable=isi1,rows=(nrow1+1),cols=(kol1),titlerows=1,selectmode="extended",colwidth=15)
          tkconfigure(tabel1,rowseparator="")
          wadah1_2=matrix(0,nrow1,kol1)
          y1=matrix(0,nrow1,kol1)
          isi1_2=tclArray()
          for (i in 0:nrow1)
          {
            for (j in 0:kol1)
            {
              isi1_2[[i,j]]=wadah1_2[i,j]
            }
          }
          isi1_2[[0,0]]="Sampel 2"
          tabel1_2=tkwidget(norm2.1,"table",variable=isi1_2,rows=(nrow1+1),cols=(kol1),titlerows=1,selectmode="extended",colwidth=15)
          tkconfigure(tabel1_2,rowseparator="",colseparator="")
          tkgrid(tabel1, tabel1_2)
          
          hitnormal = function()
          {
            tkdestroy(norm2.1)
            for(i in 1:nrow1)
            {
              for(j in 1:kol1)
              {
                x1[i,j]=as.numeric(isi1[[i,j-1]])
                y1[i,j]=as.numeric(isi1_2[[i,j-1]])
              }
            }
            y1 = as.numeric(y1)
            x1 = as.numeric(x1)
            
            a1 = y1-x1 #selisih
            b1 = a1[a1!=0]
            c1 = sort(b1) #urut selisih
            nnn1 = length(b1)
            rank1 = rank(abs(c1)) #ranking
            
            rankneg1 = rep(0,nnn1)
            rankpos1 = rep(0,nnn1)
            
            for(i in 1:nnn1)
            {
              if(b1[i]<0) {rankneg1[i]=rankneg1[i]+rank1[i]}
              else if(b1[i]>0) {rankpos1[i]=rankpos1[i]+rank1[i]}
            }
            rmin2 = sum(rankneg1)
            rpos2 = sum(rankpos1)
            w = rmin2+rpos2
            t = min(rmin2,rpos2)
            
            #-------- UJI NORMALITAS-------------
            normal1 = ks.test(x1,y1)
            normaltes1.1 = as.numeric(round(normal1[[2]],4))
            
            norm2.2 = tktoplevel(bg="Light Steel Blue")
            tktitle(norm2.2) = "UJI NORMALITAS"
            normaltes1.11 = tklabel(norm2.2,text=as.numeric(normaltes1.1))
            scr=tkscrollbar(norm2.2,orient="vertical",command=function(...)tkyview(teks1,...))
            teks1=tktext(norm2.2, bg="Light Steel Blue",width=85,height=20,yscrollcommand=function(...)tkset(scr,...))
            tkgrid(teks1,scr,sticky="news")
            tkgrid.rowconfigure(norm2.2,teks1,weight=1)
            tkgrid.columnconfigure(norm2.2,teks1,weight=1)
            tkinsert(teks1,"end",paste("
          =========================================
           HASIL UJI NORMALITAS KOLMOGOROV-SMIRNOV
          =========================================
	
          HIPOTESIS\t:
          H0\t: Data berdistribusi normal
          H1\t: Data tidak berdistribusi normal
		
          DAERAH KRITIS\t:
          Tolak Ho Jika
          P-Value < 0.05
	
          STATISTIK UJI\t:
          P-Value  = ",normaltes1.1))
            
            if((normaltes1.1) < (0.05))
            {
              tkinsert(teks1,"end",paste("
	
          KEPUTUSAN\t:
          Tolak Ho
          Karena\t",normaltes1.1,"< 0.05
    
          KESIMPULAN\t: Data tidak berdistribusi normal"))
              
              #----- Pilihan Uji -----
              mainwil1 = function()
              {
                tkdestroy(norm2.2)
                library(tcltk)
                input = tktoplevel(bg="Light Steel Blue")
                tktitle(input) = "UJI WILCOXON - INPUT DATA"
                
                ns1 = tclVar("(Sampel 1)")
                e2 = tkentry(input, width=20, textvariable=ns1)
                tkgrid(tklabel(input, text="Nama Sampel 1 :", font=teks3, bg="Light Steel Blue"),sticky="e")
                tkgrid(e2,sticky="e")
                ns2 = tclVar("(Sampel 2)")
                e3 = tkentry(input, width=20, textvariable=ns2)
                tkgrid(tklabel(input, text="Nama Sampel 2 :", font=teks3, bg="Light Steel Blue"),sticky="e")
                tkgrid(e3,sticky="e")
                alpha = tclVar("0.05")
                e4 = tkentry(input, width=20, textvariable=alpha)
                tkgrid(tklabel(input, text="Alpha :", font=teks3, bg="Light Steel Blue"),sticky="e")
                tkgrid(e4,sticky="e")
                tkgrid(tklabel(input, text=" ", bg="Light Steel Blue"))
                
                tkgrid(tklabel(input, text="Hipotesis Alternatif :", font=teks3, bg="Light Steel Blue"),sticky="e")
                rb1 = tkradiobutton(input)
                rb2 = tkradiobutton(input)
                rb3 = tkradiobutton(input)
                
                alter = tclVar("kanan")
                
                tkconfigure(rb1,variable=alter,value="kanan", bg="Light Steel Blue")
                tkconfigure(rb2,variable=alter,value="kiri", bg="Light Steel Blue")
                tkconfigure(rb3,variable=alter,value="duasisi", bg="Light Steel Blue")
                tkgrid(tklabel(input,text="Lebih besar", font=teks4, bg="Light Steel Blue"),rb1)
                tkgrid(tklabel(input,text="Lebih kecil", font=teks4, bg="Light Steel Blue"),rb2)
                tkgrid(tklabel(input,text="Tidak sama dengan", font=teks4, bg="Light Steel Blue"),rb3)
                
                tkgrid(tklabel(input, text=" ", bg="Light Steel Blue"))
                
                #------- INPUT KANAN -------
                inputkanan = function()
                {
                  if (tclvalue(alpha)==" "||tclvalue(ns1)=="(Sampel 1)"||tclvalue(ns2)=="(Sampel 2)")
                  {
                    tkmessageBox(message="Inputan Belum Lengkap",icon="warning")
                  }
                  else 
                  {
                    #--- n<30 ----
                    if(tclvalue(n1)<30)
                    {
                      hkak30.1 = tktoplevel(bg="Light Steel Blue")
                      tktitle(hkak30.1) = "Uji Wilcoxon Satu Arah Sisi Kanan"
                      ns11 = tclvalue(ns1)
                      ns21 = tclvalue(ns2)
                      nnn11 = nnn1
                      whit1 = w
                      alphaa1 = as.numeric(tclvalue(alpha))
                      wtab1 = qsignrank(alphaa1,nnn11) #daerah kritis
                      scr=tkscrollbar(hkak30.1,orient="vertical",command=function(...)tkyview(teks1,...))
                      teks1=tktext(hkak30.1, bg="Light Steel Blue",width=85,height=20,yscrollcommand=function(...)tkset(scr,...))
                      tkgrid(teks1,scr,sticky="news")
                      tkgrid.rowconfigure(hkak30.1,teks1,weight=1)
                      tkgrid.columnconfigure(hkak30.1,teks1,weight=1)
                      tkinsert(teks1,"end",paste("
                  =========================================
                   HASIL UJI WILCOXON SATU ARAH SISI KANAN
                  =========================================
	
                  HIPOTESIS\t:
                  H0\t: Tidak terdapat perbedaan antara Median ",ns11," dan Median",ns21,"
                  H1\t: Median ",ns11," lebih besar dari Median",ns21,"
		
                  DAERAH KRITIS\t:
                  Tolak Ho Jika
                  W hitung >= W Alpha
                  W hitung >= ",wtab1,"		   
	  
                  STATISTIK UJI\t:
                  W hitung = ",whit1))
                      
                      if(whit1>=wtab1)
                      {
                        tkinsert(teks1,"end",paste("
	
                  KEPUTUSAN\t:
                  Tolak Ho
                  Karena\t",whit1,">=",wtab1,
                                                   "
    
                  KESIMPULAN\t: Median ",ns11," lebih besar dari Median",ns21))
                      }
                      
                      else
                      {
                        tkinsert(teks1,"end",paste("
		
                  KEPUTUSAN\t:
                  Terima Ho
                  Karena\t",whit1,"<",wtab1,
                                                   "
      
                  KESIMPULAN\t: Tidak terdapat perbedaan antara Median ",ns11," dan Median",ns21))
                      }
                      tombol = tkbutton(hkak30.1, text="TUTUP", command=function()tkdestroy(hkak30.1))
                      tkgrid(tombol)
                    }
                    #--- n>=30 ----
                    else if(tclvalue(n1)>=30)
                    {
                      ns12 = tclvalue(ns1)
                      ns22 = tclvalue(ns2)
                      hkal30 = tktoplevel(bg="Light Steel Blue")
                      tktitle(hkal30) = "Uji Wilcoxon Satu Arah Sisi Kanan"
                      t1 = t
                      alphaa1.1 = as.numeric(tclvalue(alpha))
                      zhit1   = round(((t1-((nnn1*(nnn1+1))/4))/sqrt((nnn1*(nnn1+1)*(2*nnn1+1))/24)),4)
                      z1      = tklabel(hkal30,text=as.numeric(zhit1))
                      dk1  = round(qnorm(1-alphaa1.1),4) #daerah kritis
                      dk11 = tklabel(hkal30,text=as.numeric(dk1))
                      pvalue1 = round((1-(pnorm(zhit1))),4)
                      
                      scr=tkscrollbar(hkal30,orient="vertical",command=function(...)tkyview(teks1,...))
                      teks2=tktext(hkal30, bg="Light Steel Blue",width=85,height=20,yscrollcommand=function(...)tkset(scr,...))
                      tkgrid(teks2,scr,sticky="news")
                      tkgrid.rowconfigure(hkal30,teks2,weight=1)
                      tkgrid.columnconfigure(hkal30,teks2,weight=1)
                      tkinsert(teks2,"end",paste("
                  =========================================
                   HASIL UJI WILCOXON SATU ARAH SISI KANAN
                  =========================================
	
                  HIPOTESIS\t:
                  H0\t: Tidak terdapat perbedaan antaraMedian ",ns12," = Median",ns22,"
                  H1\t: Median ",ns12," lebih besar dari Median",ns22,"
		
                  DAERAH KRITIS\t:
                  Tolak Ho Jika
                  1. Z hitung > Z alpha
                     Z hitung > ",dk1,"		   
                  2. P-Value < Alpha
                     P-Value < ",alphaa1.1,"
	
                  STATISTIK UJI\t:
                  Z hitung = ",zhit1,"
                  P-Value  = ",pvalue1))
                      
                      if(zhit1>dk1)
                      {
                        tkinsert(teks2,"end",paste("
	
                  KEPUTUSAN\t:
                  Tolak Ho
                  Karena\t1.",zhit1,">",dk1,"
                  \t   2.",pvalue1,"<",alphaa1.1,
                                                   "
    
                  KESIMPULAN\t: Median ",ns12," lebih besar dari Median",ns22))
                      }
                      
                      else
                      {
                        tkinsert(teks2,"end",paste("
		
                  KEPUTUSAN\t:
                  Terima Ho
                  Karena\t1.",zhit1,"<",dk1,"
                    \t2.",pvalue1,">",alphaa1.1,
                                                   "
    
                  KESIMPULAN\t: Tidak terdapat perbedaan antara Median ",ns12," dan Median",ns22))
                      }
                      tombol = tkbutton(hkal30, text="TUTUP", command=function()tkdestroy(hkal30))
                      tkgrid(tombol)
                    }
                  }
                  
                }
                #------- INPUT KIRI -------
                inputkiri = function()
                {
                  if (tclvalue(alpha)==" "||tclvalue(ns1)=="(Sampel 1)"||tclvalue(ns2)=="(Sampel 2)")
                  {
                    tkmessageBox(message="Inputan Belum Lengkap",icon="warning")
                  }
                  else 
                  {
                    #--- n<30 ----
                    if(tclvalue(n1)<30)
                    {
                      hkik30.1 = tktoplevel(bg="Light Steel Blue")
                      tktitle(hkik30.1) = "Uji Wilcoxon Satu Arah Sisi Kiri"
                      ns11 = tclvalue(ns1)
                      ns21 = tclvalue(ns2)
                      nnn11 = nnn1
                      whit2 = w
                      alphaa2 = as.numeric(tclvalue(alpha))
                      wtab2 = nnn11*(2*nnn11+1)-qsignrank(alphaa2,nnn11) #daerah kritis
                      scr=tkscrollbar(hkik30.1,orient="vertical",command=function(...)tkyview(teks1,...))
                      teks1=tktext(hkik30.1, bg="Light Steel Blue",width=85,height=20,yscrollcommand=function(...)tkset(scr,...))
                      tkgrid(teks1,scr,sticky="news")
                      tkgrid.rowconfigure(hkik30.1,teks1,weight=1)
                      tkgrid.columnconfigure(hkik30.1,teks1,weight=1)
                      tkinsert(teks1,"end",paste("
                  =========================================
                   HASIL UJI WILCOXON SATU ARAH SISI KIRI
                  =========================================
	
                  HIPOTESIS\t:
                  H0\t: Tidak terdapat perbedaan antara Median ",ns11," dan Median",ns21,"
                  H1\t: Median ",ns11," lebih kecil dari Median",ns21,"
		
                  DAERAH KRITIS\t:
                  Tolak Ho Jika
                  W hitung <= n*(2*n+1) - W Alpha
                  W hitung <= ",wtab2,"		   
	  
                  STATISTIK UJI\t:
                  W hitung = ",whit2))
                      
                      if(whit2<=wtab2)
                      {
                        tkinsert(teks1,"end",paste("
	
                  KEPUTUSAN\t:
                  Tolak Ho
                  Karena\t",whit2,"<=",wtab2,
                                                   "
    
                  KESIMPULAN\t: Median ",ns11," lebih kecil dari Median",ns21))
                      }
                      
                      else
                      {
                        tkinsert(teks1,"end",paste("
		
                  KEPUTUSAN\t:
                  Terima Ho
                  Karena\t",whit2,">",wtab2,
                                                   "
      
                  KESIMPULAN\t: Tidak terdapat perbedaan antara Median ",ns11," dan Median",ns21))
                      }
                      tombol = tkbutton(hkik30.1, text="TUTUP", command=function()tkdestroy(hkik30.1))
                      tkgrid(tombol)
                    }
                    #--- n>=30 ----
                    else if(tclvalue(n1)>=30)
                    {
                      ns12 = tclvalue(ns1)
                      ns22 = tclvalue(ns2)
                      hkil30 = tktoplevel(bg="Light Steel Blue")
                      tktitle(hkil30) = "Uji Wilcoxon Satu Arah Sisi Kiri"
                      t2 = t
                      alphaa2.1 = as.numeric(tclvalue(alpha))
                      zhit2   = round(((t2-((nnn1*(nnn1+1))/4))/sqrt((nnn1*(nnn1+1)*(2*nnn1+1))/24)),4)
                      z2      = tklabel(hkil30,text=as.numeric(zhit2))
                      dk2  = round(qnorm(1-alphaa2.1),4) #daerah kritis
                      dk21 = tklabel(hkil30,text=as.numeric(dk2))
                      pvalue2 = round((1-(pnorm(zhit2))),4)
                      
                      scr=tkscrollbar(hkil30,orient="vertical",command=function(...)tkyview(teks1,...))
                      teks2=tktext(hkil30, bg="Light Steel Blue",width=85,height=20,yscrollcommand=function(...)tkset(scr,...))
                      tkgrid(teks2,scr,sticky="news")
                      tkgrid.rowconfigure(hkil30,teks2,weight=1)
                      tkgrid.columnconfigure(hkil30,teks2,weight=1)
                      tkinsert(teks2,"end",paste("
                  =========================================
                   HASIL UJI WILCOXON SATU ARAH SISI KIRI
                  =========================================
	
                  HIPOTESIS\t:
                  H0\t: Tidak terdapat perbedaan antara Median ",ns12," dan Median",ns22,"
                  H1\t: Median ",ns12," lebih kecil dari Median",ns22,"
		
                  DAERAH KRITIS\t:
                  Tolak Ho Jika
                  1. Z hitung < Z alpha
                     Z hitung < ",(-dk2),"		   
                  2. P-Value < Alpha
                     P-Value < ",alphaa2.1,"
	
                  STATISTIK UJI\t:
                  Z hitung = ",zhit2,"
                  P-Value  = ",pvalue2))
                      
                      if(zhit2<(-dk2))
                      {
                        tkinsert(teks2,"end",paste("
	
                  KEPUTUSAN\t:
                  Tolak Ho
                  Karena\t2.",zhit2,"<",(-dk2),"
                  \t   2.",pvalue2,"<",alphaa2.1,
                                                   "
    
                  KESIMPULAN\t: Median ",ns12," lebih kecil dari Median",ns22))
                      }
                      
                      else
                      {
                        tkinsert(teks2,"end",paste("
		
                  KEPUTUSAN\t:
                  Terima Ho
                  Karena\t2.",zhit2,">",(-dk2),"
                    \t2.",pvalue2,">",alphaa2.1,
                                                   "
    
                  KESIMPULAN\t: Tidak terdapat perbedaan antara Median ",ns12," dan Median",ns22))
                      }
                      tombol = tkbutton(hkil30, text="TUTUP", command=function()tkdestroy(hkil30))
                      tkgrid(tombol)
                    }
                  }
                }
                #------- INPUT DUA SISI -------
                inputduasisi = function()
                {
                  if (tclvalue(alpha)==" "||tclvalue(ns1)=="(Sampel 1)"||tclvalue(ns2)=="(Sampel 2)")
                  {
                    tkmessageBox(message="Inputan Belum Lengkap",icon="warning")
                  }
                  else 
                  {
                    #--- n<30 ----
                    if(tclvalue(n1)<30)
                    {
                      hdsk30.1 = tktoplevel(bg="Light Steel Blue")
                      tktitle(hdsk30.1) = "Uji Wilcoxon Dua Arah"
                      ns11 = tclvalue(ns1)
                      ns21 = tclvalue(ns2)
                      nnn11 = nnn1
                      whit3 = w
                      alphaa3 = as.numeric(tclvalue(alpha))
                      wtab31 = qsignrank(alphaa3/2,nnn11) #daerah kritis
                      wtab32 = nnn11*(2*nnn11+1)-wtab31 #daerah kritis
                      scr=tkscrollbar(hdsk30.1,orient="vertical",command=function(...)tkyview(teks1,...))
                      teks1=tktext(hdsk30.1, bg="Light Steel Blue",width=85,height=20,yscrollcommand=function(...)tkset(scr,...))
                      tkgrid(teks1,scr,sticky="news")
                      tkgrid.rowconfigure(hdsk30.1,teks1,weight=1)
                      tkgrid.columnconfigure(hdsk30.1,teks1,weight=1)
                      tkinsert(teks1,"end",paste("
                  =========================================
                         HASIL UJI WILCOXON DUA ARAH
                  =========================================
	
                  HIPOTESIS\t:
                  H0\t: Tidak terdapat perbedaan antara Median ",ns11," dan Median",ns21,"
                  H1\t: Terdapat perbedaan antara Median ",ns11," dan Median",ns21,"
		
                  DAERAH KRITIS\t:
                  Tolak Ho Jika
                  W hitung >= W alpha/2
                  W hitung >= ",wtab31,"
                  atau
                  W hitung <= n*(2*n+1) - W Alpha
                  W hitung <= ",wtab32,"		   
	  
                  STATISTIK UJI\t:
                  W hitung = ",whit3))
                      
                      if(whit3>=wtab31||whit3<=wtab32)
                      {
                        tkinsert(teks1,"end",paste("
	
                  KEPUTUSAN\t:
                  Tolak Ho
                  Karena\t",whit3,">=",wtab31,"
                  \t  atau ",whit3,"<=",wtab32,
                                                   "
    
                  KESIMPULAN\t: Terdapat perbedaan antara Median ",ns11," dan Median",ns21))
                      }
                      
                      else
                      {
                        tkinsert(teks1,"end",paste("
		
                  KEPUTUSAN\t:
                  Terima Ho
                  Karena\t",whit3,"<=",wtab31,"
                  \t  atau ",whit3,">=",wtab32,
                                                   "
      
                  KESIMPULAN\t: Tidak terdapat perbedaan antara Median ",ns11," dan Median",ns21))
                      }
                      tombol = tkbutton(hdsk30.1, text="TUTUP", command=function()tkdestroy(hdsk30.1))
                      tkgrid(tombol)
                    }
                    #--- n>=30 ----
                    else if(tclvalue(n1)>=30)
                    {
                      ns12 = tclvalue(ns1)
                      ns22 = tclvalue(ns2)
                      hdsl30 = tktoplevel(bg="Light Steel Blue")
                      tktitle(hdsl30) = "Uji Wilcoxon Dua Arah"
                      t3 = t
                      alphaa3.1 = as.numeric(tclvalue(alpha))
                      zhit3   = round(((t3-((nnn1*(nnn1+1))/4))/sqrt((nnn1*(nnn1+1)*(2*nnn1+1))/24)),4)
                      z3      = tklabel(hdsl30,text=as.numeric(zhit3))
                      dk3  = round(qnorm(1-alphaa3.1/2),4) #daerah kritis
                      dk33 = tklabel(hdsl30,text=as.numeric(dk3))
                      pvalue3 = round((1-(pnorm(zhit3))),4)
                      
                      scr=tkscrollbar(hdsl30,orient="vertical",command=function(...)tkyview(teks1,...))
                      teks2=tktext(hdsl30, bg="Light Steel Blue",width=85,height=20,yscrollcommand=function(...)tkset(scr,...))
                      tkgrid(teks2,scr,sticky="news")
                      tkgrid.rowconfigure(hdsl30,teks2,weight=1)
                      tkgrid.columnconfigure(hdsl30,teks2,weight=1)
                      tkinsert(teks2,"end",paste("
                  =========================================
                         HASIL UJI WILCOXON DUA ARAH
                  =========================================
	
                  HIPOTESIS\t:
                  H0\t: Tidak terdapat perbedaan antara Median ",ns12," dan Median",ns22,"
                  H1\t: Terdapat perbedaan antara Median ",ns12," dan Median",ns22,"
		
                  DAERAH KRITIS\t:
                  Tolak Ho Jika
                  1. |Z hitung| > Z alpha
                     |Z hitung| > ",dk3,"		   
                  2. P-Value < Alpha
                     P-Value < ",alphaa3.1,"
	
                  STATISTIK UJI\t:
                  Z hitung = ",zhit3,"
                  P-Value  = ",pvalue3))
                      
                      if(abs(zhit3)>dk3)
                      {
                        tkinsert(teks2,"end",paste("
	
                  KEPUTUSAN\t:
                  Tolak Ho
                  Karena\t1.|",zhit3,"| > ",dk3,"
                  \t   2.",pvalue3,"<",alphaa3.1,
                                                   "
    
                  KESIMPULAN\t: Terdapat perbedaan antara Median ",ns12," dan Median",ns22))
                      }
                      
                      else
                      {
                        tkinsert(teks2,"end",paste("
		
                  KEPUTUSAN\t:
                  Terima Ho
                  Karena\t1.|",zhit3,"| < ",dk3,"
                    \t2.",pvalue3,">",alphaa3.1,
                                                   "
    
                  KESIMPULAN\t: Tidak terdapat perbedaan antara Median ",ns12," dan Median",ns22))
                      }
                      tombol = tkbutton(hdsl30, text="TUTUP", command=function()tkdestroy(hdsl30))
                      tkgrid(tombol)
                    }
                  }
                  
                }
                ok1 = function()
                {
                  altern = as.character(tclvalue(alter))
                  if(altern=="kanan")
                    inputkanan()
                  else if(altern=="kiri")
                    inputkiri()
                  else if(altern=="duasisi")
                    inputduasisi()
                }
                tombol0 = tkbutton(input, text="UJI WILCOXON", command=ok1)
                tkgrid(tombol0)
                tombol = tkbutton(input, text="KEMBALI", command=function()tkdestroy(input))
                tkgrid(tombol)
              }
              tombol1 = tkbutton(norm2.2, text="LANJUTKAN", command=mainwil1)
              tkgrid(tombol1)
            }
            else
            {
              tkinsert(teks1,"end",paste("
		
          KEPUTUSAN\t:
          Terima Ho
          Karena\t",normaltes1.1,"> 0.05
    
          KESIMPULAN\t: Data berdistribusi normal"))
              
              tutup = function()
              {
                tkmessageBox(type="ok",message="Maaf, data tidak cocok untuk UJI WILCOXON\ngunakan data lainnya",icon="warning")
                tkdestroy(norm2.2)
              }
              tombol2 = tkbutton(norm2.2, text="LANJUTKAN", command=tutup)
              tkgrid(tombol2) 
            }
          }
          tkgrid(tklabel(norm2.1, text="", bg="Light Steel Blue"))
          tombol3 = tkbutton(norm2.1, text="UJI NORMALITAS", command=hitnormal, bg="Gainsboro")
          tkgrid(tombol3)
        }
      }
      tombol4 = tkbutton(norm2, text="OK", command=ok, bg="Gainsboro")
      tkgrid(tombol4)
      tombol5 = tkbutton(norm2, text="KEMBALI", command=function()tkdestroy(norm2), bg="Gainsboro")
      tkgrid(tombol5)
    }
    
    ######## BANTUAN ########
    #------- PETUNJUK ------
    petunjuk = function()
    {
      pet = tktoplevel(bg="Lavender")
      tktitle(pet) = "PETUNJUK PENGGUNAAN"
      tekss6 = tkfont.create(family="times",size=18,weight="bold")
      tkgrid(tklabel(pet, text="PETUNJUK PENGGUNAAN",font=tekss6,bg="Antique White"))
      
      tkgrid(tklabel(pet, text="1. Klik Uji Wilcoxon untuk melakukan pengujian",font=teks4,bg="Lavender"),sticky="w")
      tkgrid(tklabel(pet, text="2. Pilih salah satu submenu yaitu Impor Data atau Input Data",font=teks4,bg="Lavender"),sticky="w")
      tkgrid(tklabel(pet, text="    a. Untuk Impor Data, sesuaikan data yang akan digunakan dengan ketentuan",font=teks4,bg="Lavender"),sticky="w")
      tkgrid(tklabel(pet, text="        Pertama, data yang dipilih akan otomatis diuji normalitas, setelah muncul hasil klik LANJUTKAN.",font=teks4,bg="Lavender"),sticky="w")
      tkgrid(tklabel(pet, text="        Jika data tidak berdistribusi normal, akan muncul jendela baru untuk dilanjutkan ke UJI WILCOXON.",font=teks4,bg="Lavender"),sticky="w")
      tkgrid(tklabel(pet, text="        Masukkan nama untuk sampel 1 dan sampel 2, alpha, dan pilih hipotesis alternatif yang sesuai, klik UJI WILCOXON.",font=teks4,bg="Lavender"),sticky="w")
      tkgrid(tklabel(pet, text="        Hasil uji akan menampilkan hipotesis sesuai nama sampel dan hipotesis alternatif yang dipilih, daerah kritis,",font=teks4,bg="Lavender"),sticky="w")
      tkgrid(tklabel(pet, text="        statistik uji, keputusan, serta kesimpulan berdasarkan perhitungan.",font=teks4,bg="Lavender"),sticky="w")
      tkgrid(tklabel(pet, text="        Namun jika data berdistribusi normal, akan muncul pesan bahwa data yang digunakan tidak cocok untuk UJI WILCOXON",font=teks4,bg="Lavender"),sticky="w")
      tkgrid(tklabel(pet, text="        dan akan kembali ke menu utama.",font=teks4,bg="Lavender"),sticky="w")
      tkgrid(tklabel(pet, text="    b. Untuk Input Data, masukkan banyak data yang akan diinput, lalu klik OK.",font=teks4,bg="Lavender"),sticky="w")
      tkgrid(tklabel(pet, text="        Inputkan data yang akan digunakan di masing-masing kolom, klik UJI NORMALITAS,",font=teks4,bg="Lavender"),sticky="w")
      tkgrid(tklabel(pet, text="        setelah muncul hasil, klik LANJUTKAN.",font=teks4,bg="Lavender"),sticky="w")
      tkgrid(tklabel(pet, text="        Jika data tidak berdistribusi normal, akan muncul jendela baru untuk dilanjutkan ke UJI WILCOXON.",font=teks4,bg="Lavender"),sticky="w")
      tkgrid(tklabel(pet, text="        Masukkan nama untuk sampel 1 dan sampel 2, alpha, dan pilih hipotesis alternatif yang sesuai, klik UJI WILCOXON.",font=teks4,bg="Lavender"),sticky="w")
      tkgrid(tklabel(pet, text="        Hasil uji akan menampilkan hipotesis sesuai nama sampel dan hipotesis alternatif yang dipilih, daerah kritis,",font=teks4,bg="Lavender"),sticky="w")
      tkgrid(tklabel(pet, text="        statistik uji, keputusan, serta kesimpulan berdasarkan perhitungan.",font=teks4,bg="Lavender"),sticky="w")
      tkgrid(tklabel(pet, text="        Namun jika data berdistribusi normal, akan muncul pesan bahwa data yang digunakan tidak cocok untuk UJI WILCOXON",font=teks4,bg="Lavender"),sticky="w")
      tkgrid(tklabel(pet, text="        dan akan kembali ke menu utama.",font=teks4,bg="lavender"),sticky="w")
      tombol4 = tkbutton(pet, text="TUTUP", command=function()tkdestroy(pet), bg="Gainsboro")
      tkgrid(tombol4)
    }
    #------- TEORI ---------
    teori = function()
    {
      library(tcltk)
      teori1 = tktoplevel(bg="Lavender")
      tktitle(teori1) = "TEORI UJI WILCOXON"
      tekss6 = tkfont.create(family="times",size=18,weight="bold")
      tkgrid(tklabel(teori1, text="TEORI UJI WILCOXON",font=tekss6,bg="Antique White"))
      tkgrid(tklabel(teori1,text="Uji Wilcoxon diperkenalkan pertama kali oleh ahli statistika bernama Frank Wilcoxon.
      Digunakan untuk menguji perbedaan dua sampel berpasangan jika data berskala ordinal.
      Uji Wilcoxon merupakan pengembangan dari Uji Sign. Perbedaannya, pada Uji Sign hanya berdasarkan pada
      arah dari perbedaan pasangan, sedangkan pada Uji Wilcoxon nilai relatif dan arah perbedaan sudah
      dipertimbangkan, sehingga pada uji ini telah memberikan bobot lebih untuk setiap pasangan.",font=teks4,bg="Lavender"))
      scr=tkscrollbar(teori1,orient="vertical",command=function(...)tkyview(teks1,...))
      teks2=tktext(teori1, bg="Lavender",width=85,height=20,yscrollcommand=function(...)tkset(scr,...))
      tkgrid(teks2,scr,sticky="news")
      tkgrid.rowconfigure(teori1,teks2,weight=1)
      tkgrid.columnconfigure(teori1,teks2,weight=1)
      tkinsert(teks2,"end",paste("
      A. Untuk Sampel Kecil (n<30) :
         Whitung = jumlah Rank
         DAERAH KRITIS:
         1. Satu Arah Sisi Kanan
            Tolak H0, jika Whitung >= Walpha
         2. Satu Arah Sisi Kiri
            Tolak H0, jika Whitung <= n(2n+1) - Walpha
         3. Dua Arah
            Tolak H0, jika Whitung >= W(alpha/2) atau Whitung <= n(2n+1) - W(alpha/2)
      B. Untuk Sampel Besar (n>=30) :
         Untuk sampel besar, perhitungan statistik uji didekati dengan uji Z
         STATISTIK UJI:
         Zhitung = (T-[(1/4)N(N+1)])/((1/24)N(N+1)(2N+1))
         dengan:
         T adalah nilai minimum dari ranking positif dan negatif
         N adalah jumlah sampel
         DAERAH KRITIS:
         1. Satu Arah Sisi Kanan
            Tolak H0, jika Zhitung > Zalpha
         2. Satu Arah Sisi Kiri
            Tolak H0, jika Whitung < -Zalpha
         3. Dua Arah
            Tolak H0, jika |Whitung| > Z(alpha/2)"))
      tombol4 = tkbutton(teori1, text="TUTUP", command=function()tkdestroy(teori1), bg="Gainsboro")
      tkgrid(tombol4)
    }
    #------- CONTOH SOAL ---------
    cs = function()
    {
      cs1 = tktoplevel(bg="Lavender")
      tktitle(cs1) = "CONTOH SOAL"
      tekss6 = tkfont.create(family="times",size=18,weight="bold")
      tkgrid(tklabel(cs1, text="CONTOH SOAL 1",font=tekss6,bg="Antique White"))
      tkgrid(tklabel(cs1, text=" ",font=teks4,bg="Lavender"))
      tkgrid(tklabel(cs1, text=" Menurut Kholifah (2020), penggunaan alat peraga ular tangga dalam pembelajaran dapat meningkatkan",font=teks4,bg="Lavender"))
      tkgrid(tklabel(cs1, text=" hasil belajar matematika peserta didik. Penelitian untuk membuktikan hal tersebut dilakukan pada",font=teks4,bg="Lavender"))
      tkgrid(tklabel(cs1, text=" 28 siswa kelas 4 MIN 3 Metro Pusat. Penelitian dilakukan dengan memberikan Pre Test terkait luas",font=teks4,bg="Lavender"))
      tkgrid(tklabel(cs1, text=" bangun datar kemudian peserta didik diberikan pembelajaran dengan menggunakan alat peraga ular tangga.",font=teks4,bg="Lavender"))
      tkgrid(tklabel(cs1, text=" Setelah itu, peserta didik tersebut diberikan Post Test dengan soal yang sama dengan Pre Test sebelumnya.",font=teks4,bg="Lavender"))
      tkgrid(tklabel(cs1, text=" Buktikanlah apakah benar penggunaan alat peraga ular tangga dalam pembelajaran dapat meningkatkan hasil",font=teks4,bg="Lavender"))
      tkgrid(tklabel(cs1, text=" belajar peserta didik dengan membandingkan hasil Pre Test dan Post Test siswa kelas 4 MIN 3 Metro Pusat.",font=teks4,bg="Lavender"))
      tkgrid(tklabel(cs1, text=" ",font=teks4,bg="Lavender"))
      soal2 = function()
      {
        tkdestroy(cs1)
        cs2 = tktoplevel(bg="Lavender")
        tktitle(cs2) =  "CONTOH SOAL"
        tkgrid(tklabel(cs2, text="CONTOH SOAL 2",font=tekss6,bg="Antique White"))
        tkgrid(tklabel(cs2, text=" ",font=teks4,bg="Lavender"),sticky="w")
        tkgrid(tklabel(cs2, text=" Dilansir dari Portal Informasi Indonesia indonesia.go.id, pada 2 Maret 2020 terdapat dua Warga Negara Indonesia",font=teks4,bg="Lavender"))
        tkgrid(tklabel(cs2, text=" (WNI) yang berdomisili di Depok diketahui terinfeksi Corona Virus Disease (COVID-19). Sejak kasus pertama Covid-19",font=teks4,bg="Lavender"))
        tkgrid(tklabel(cs2, text=" muncul di Indonesia, pemerintah melakukan pembatasan yang disebut lockdown kepada masyarakat Indonesia. Kebijakan",font=teks4,bg="Lavender"))
        tkgrid(tklabel(cs2, text=" tersebut juga berdampak pada bidang pariwisata. Terjadi penurunan drastis dalam jumlah kunjungan wisatawan",font=teks4,bg="Lavender"))
        tkgrid(tklabel(cs2, text=" mancanegara (wisman) ke Indonesia setelah adanya kasus Covid-19. Untuk mendukung penelitian, digunakan data kunjungan",font=teks4,bg="Lavender"))
        tkgrid(tklabel(cs2, text=" wisman 12 bulan sebelum kasus pertama Covid-19 (Maret 2020) sebagai sebelum Covid-19 dan sesudah Covid-19 menggunakan",font=teks4,bg="Lavender"))
        tkgrid(tklabel(cs2, text=" 12 bulan setelah kasus pertama Covid-19.",font=teks4,bg="Lavender"))
        tkgrid(tklabel(cs2, text=" ",font=teks4,bg="Lavender"))
        tombol4 = tkbutton(cs2, text="TUTUP", command=function()tkdestroy(cs2), bg="Gainsboro")
        tkgrid(tombol4)
        tombol = tkbutton(cs2, text="KEMBALI", command=function()
          { cs()
          tkdestroy(cs2)}, bg="Gainsboro")
        tkgrid(tombol)
      }
      tombol4 = tkbutton(cs1, text="BERIKUTNYA", command=soal2, bg="Gainsboro")
      tkgrid(tombol4)
    }
      
    #################################### MENU #####################################
    tkadd(topmenu, "cascade", label="UJI WILCOXON", menu=menu0)
    tkadd(menu0, "cascade", label="Impor Data", menu=submenu)
      tkadd(submenu, "command", label="File Excel", command=excel)
      tkadd(submenu, "command", label="File SPSS", command=spss)
    tkadd(menu0, "command", label="Input Data", command=inputwilcoxon)
    tkadd(topmenu, "cascade", label="BANTUAN", menu=menu1)
    tkadd(menu1, "command", label="Petunjuk", command=petunjuk)
    tkadd(menu1, "command", label="Teori", command=teori)
    tkadd(menu1, "command", label="Contoh Soal", command=cs)
  }
  tombol5 = tkbutton(frame7, text="HOME", command=home,bg="Antique White")
  tkpack(tklabel(frame7,text="Silahkan Klik Home untuk masuk ke Menu Utama ",font=teks1,bg="Antique White"))
  tkpack(frame7,fill="both")
  tkpack(tombol5)
  tkpack(tklabel(frame7, text=" ",bg="Antique White"))
  }
  tombol = tkbutton(frame, text="LANJUT", command=mu,bg="Light Blue")
  tkpack(frame,fill="both")
  tkpack(tombol,ipadx=50,ipady=3,pady=6,side="bottom")
}