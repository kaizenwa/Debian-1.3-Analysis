
function cut(s) {
   while ((substr(s,1,1)==" ")||(substr(s,1,1)=="\t")) 
      s=substr(s,2,length(s)-1);
   while ((substr(s,length(s),1)==" ")||(substr(s,length(s),1)=="\t")) 
      s=substr(s,1,length(s)-1);
   return s;	
}

function rest(n)
{
   save = $0;
   $0 = cut($0);
   for (i=0; i<n-1; i++)
     $0 = cut(substr($0,length($1)+1,length($0)));
   val = $0;
   $0 = save;
   return val;
}

function fill(s,l)
{
   while ( length(s) < l ) 
      s = s " ";
   return s;
}

function conv(entrytext)
{
   gsub( "@[A-Za-z][A-Za-z0-9]*", "", entrytext );
   gsub( "\{", "", entrytext );
   gsub( "\}", "", entrytext );
   gsub( ",", "", entrytext );
   gsub( /\./, "-", entrytext );
   gsub( ";", "", entrytext );
   gsub( "  ", " ", entrytext );
   return entrytext;
}

BEGIN {
   curmenu=0;
   curlevel=0;
   menustack[curlevel]=0;
   count[menustack[curlevel]]=0;
}

NR == 2 {
   print "@c %** START OF HEADER";
   print "@setfilename " INFO;
   print "@c %** END OF HEADER";
}

{
   if ( $1=="@SUB" ) {
      entrytext = conv(rest(2));
      if ( cut(entrytext)=="" ) {
         getline;
         if ( $1=="@LABEL" ) {
            tmp = $2;
            getline;
            entrytext = $0;
            labels[tmp] = entrytext;
         } else {
            entrytext = $0;
         }
      }

      entry = fill("* " entrytext "::",24);
      
      if ( firstnode=="" ) firstnode=entrytext;
      menuentry[menustack[curlevel] count[menustack[curlevel]]] = entry;
      count[menustack[curlevel]]++;

      curlevel++;
      curmenu++;
      menustack[curlevel] = curmenu;
      count[menustack[curlevel]]=0;
   }
   else if ( $1=="@ENDSUB" ) {
      curlevel--;
   }
   else if ( $1=="@LABEL" ) {
      labels[$2] = entrytext;
   }
   else if ( $1=="@title" ) {
      title = rest(2);
   }
   else if ( $1=="@subtitle" ) {
      subtitle = rest(2);
   }
   else if ( $1=="@author" ) {
      author = rest(2);
   }
}

END {
   close(FILENAME);

   curmenu=0;
   curlevel=0;
   firstsec=1;

   while ( getline < FILENAME )
   {
      if ( $1=="@TOP" ) {
	 print "@node top";
	 print "@top " title;
	 print "@example";
	 print author;
	 print subtitle;
	 print "@end example";
      }
      else if ( ( $1=="@title" ) || ( $1=="@subtitle" ) || ( $1=="@author" ) ||
	       ( $1=="@LABEL" ) ) 
      {
      }
      else if ( $1=="@REF" ) {
	 print "@ref{" labels[$2] "}";
      }
      else if ( $1=="@SUB" ) {
         if ( firstsec==1 ) {
	    if ( count[menustack[curlevel]] ) {
	       print "@menu";
	       for (i=0; i<count[menustack[curlevel]]; i++) 
	          print menuentry[menustack[curlevel] i];
   	       print "@end menu";
	    }
	 }

	 s = conv(rest(2));
         if ( s=="" ) {
            getline < FILENAME;
            if ( $1=="@LABEL" ) {
               getline < FILENAME;
               s = $0;
            } else {
               s = $0;
            }
         }

	 print "@node " s;
	 
	 if ( curlevel==0 ) print "@chapter " s;
	 else if ( curlevel==1 ) print "@section " s;
	 else if ( curlevel==2 ) print "@subsection " s;
	 else print "@subsubsection " s;

	 curlevel++;
	 curmenu++;
	 menustack[curlevel] = curmenu;
         firstsec=1;
      }
      else if ( $1=="@ENDSUB" ) {
	 curlevel--;
         firstsec=0;
      }
      else print;
   }
}
