#define range(f,l) Int(Rnd*((l+1)-(f)))+(f)
Dim Shared As Single spread=25,scale=.76
Sub Tree(x1 As Single,y1 As Single,size As Single,angle As Single,depth As Single,colb As Ulong=0,colL As Ulong=0,im As Any Ptr=0)
      #define incircle(cx,cy,radius,x,y) (cx-x)*(cx-x) +(cy-y)*(cy-y)<= radius*radius
      Var x2=x1-.25*size*Cos(angle*.01745329)
      Var y2=y1-.25*size*Sin(angle*.01745329)
      Static As Long count,fx,fy,sz,z
      If count=0 Then  fx=x1:fy=y1:sz=size:z=2^(depth+1)-1
      Line im,(x1,y1)-(x2,y2),colb
      If count=0 Then  fx=x2:fy=y2:sz=size
      count=count+1
      If count>z Then count=0
      If incircle(fx,fy,(.45*sz),x2,y2)=0 Then Circle im,(x2,y2),.01*sz,colL 
      If depth>0 Then
            Tree(x2, y2, size * Scale, angle - Spread, depth - 1,colB,colL,im)
            Tree(x2, y2, size * Scale, angle + Spread, depth - 1,colB,colL,im)
      End If
End Sub

Sub sky( im As Any Ptr)
      #define map(a,b,x,c,d) ((d)-(c))*((x)-(a))/((b)-(a))+(c)
      Dim As Long x,y
      Imageinfo im, x,y
      For z As Long=0 To y+5
            Var r=map(0,(y+5),z,0,250)
            Var g=map(0,(y+5),z,0,250)
            Var b=map(0,(y+5),z,200,250)
            Line im,(0,z)-(x,z),Rgb(r,g,b)
      Next z
End Sub

Sub wall( im As Ulong Ptr,xres As Long,yres As Long,bw As Long,bh As Long)
      Randomize 1
      Var k=bw/4
      For y As Long=-1 To yres Step bh
            For x As Long=-bw To xres Step bw
                  Line im,(x+k,y)-Step(bw,bh),Rgb(200,100+(Rnd*15-Rnd*15),0),bf
                  Line im,(x+k,y)-Step(bw,bh),Rgb(200,200,200),b
            Next x
            k=-k
      Next y
End Sub

Sub trees( im As Ulong Ptr,sz As Long,stp As Long,r As Long)
      Randomize r
      Dim As Long x,y
      Imageinfo im,x,y
      For n As Long=.5*sz+15 To x-(.5*sz+15) Step stp
            Tree(n,y-range(0,10),sz+range(-5,sz/2),range(80,120),12,Rgb(range(190,210),range(80,120),0),Rgb(range(0,90),100,range(0,50)),im)'30
      Next
End Sub

Sub ground( im As Any Ptr,dy As Long)
      Dim As Long x,y
      Imageinfo im,x,y
      Line im,(0,y)-(x,y-dy),Rgb(90,50,0),bf
      spread=10
      For n As Long=1 To 50
            Var x=range(0,x),y=range((y-10),y)
            Circle im,(x,y),15,Rgb(60,35,0),,,.5,f
            tree(x,y,20,range(80,120),12,Rgb(range(190,210),range(80,120),0),Rgb(range(0,90),100,range(0,50)),im)
      Next
      spread=25
End Sub

Function sz(im As Any Ptr) As Long
      Dim As Long size
      Imageinfo im,,,,,,size
      Return size
End Function

Function Regulate(Byval MyFps As Long,Byref fps As Long) As Long
      Static As Double timervalue,lastsleeptime,t3,frames
      Var t=Timer
      frames+=1
      If (t-t3)>=1 Then t3=t:fps=frames:frames=0
      Var sleeptime=lastsleeptime+((1/myfps)-T+timervalue)*1000
      If sleeptime<1 Then sleeptime=1
      lastsleeptime=sleeptime
      timervalue=T
      Return sleeptime
End Function


#macro Sweep(p,sz,start,speed)
Scope
      Var f=p[start]
      For z As Long =start+speed To (sz)
            p[z-speed]=p[z]
      Next z
      p[sz-1]=f
End Scope
#endmacro

#macro redo(a,c,sz)
For n As Long=0 To sz
      a[n]=c[n]
Next n
#endmacro

#define ic(h) imagecreate(xres,h)
Screenres 800,600,32
Color ,Rgb(0,100,0)
Dim As Long xres,yres
Screeninfo xres,yres
Dim As Ulong Ptr i(1 To 8)={ic(.4*yres),ic(.4*yres),ic(.4*yres),ic(.4*yres),ic(.4*yres),ic(.4*yres),ic(.4*yres),ic(.4*yres)}
Dim As Ulong count(1 To 4),size(1 To 4)
Dim As Long fps

sky(i(7))
sky(i(8))
ground(i(7),15)
ground(i(8),15)
trees(i(7),40,25,1)
trees(i(8),40,25,1)
size(4)=sz(i(7))

ground(i(5),60)
ground(i(6),60)
trees(i(5),80,30,2)
trees(i(6),80,30,2)
size(3)=sz(i(5))

ground(i(3),60)
ground(i(4),60)
trees(i(3),120,40,3)
trees(i(4),120,40,3)
size(2)=sz(i(3))

wall(i(1),xres,yres,xres\20,xres\40)
wall(i(2),xres,yres,xres\20,xres\40)
size(1)=sz(i(1))

redo(i(7),i(8),size(4)\4)
redo(i(5),i(6),size(3)\4)
redo(i(3),i(4),size(2)\4)
redo(i(1),i(2),size(1)\4)

Do
      count(4)+=1
      count(3)+=2
      count(2)+=3
      count(1)+=6
      Screenlock
      Cls
      Sweep(i(7),size(4)\4,8,1)
      Sweep(i(5),size(3)\4,8,2)
      Sweep(i(3),size(2)\4,8,3)
      Sweep(i(1),size(1)\4,8,6)
      Put(0,0),i(7),Pset
      Put(0,70),i(5),trans
      Put(0,130),i(3),trans
      Put(0,330),i(1),trans
      Draw string (1,1),str(fps),rgb(100,100,100)
      Screenunlock
      If count(4) >= xres Then:count(4)=0:redo(i(7),i(8),size(4)\4):End If
      If count(3) >= xres Then:count(3)=0:redo(i(5),i(6),size(3)\4):End If
      If count(2) >= xres Then:count(2)=0:redo(i(3),i(4),size(2)\4):End If
      If count(1) >= xres Then:count(1)=0:redo(i(1),i(2),size(1)\4):End If
      Sleep regulate(40,fps)
Loop Until Len(Inkey)
For n As Long=1 To 8
      Imagedestroy(i(n))
Next
