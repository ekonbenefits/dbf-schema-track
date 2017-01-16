
#define K_DIR_ARG    "-dir="

PROCEDURE CreateStructDirIfMissing()
    path := "struct"
    IF(!FILE(path))
        MakeDir("struct")
    ENDIF

PROCEDURE MAIN(...)
    LOCAL args := HB_AParams()
    LOCAL dirs := {}
    LOCAL prefixes := {}
    LOCAL indexes := {".NT*",".ND*", ".CD*",".MD*"}
    LOCAL tmp, dir, prefix, file, ifile, fld, entry, suffix

    FOR EACH tmp IN args
        DO CASE
        CASE Lower(Left(tmp, Len(K_DIR_ARG))) == K_DIR_ARG
            AAdd(dirs, SubStr(tmp,Len(K_DIR_ARG) + 1))
        OTHERWISE
            AAdd(prefixes, tmp)
        ENDCASE
    NEXT

    if EMPTY(dirs)
      AAdd(dirs, "")
    end if

    for each dir in dirs
        for each prefix in prefixes
            look := dir + prefix + "*.DBF"
            ? "Prefix: ", look
            CreateStructDirIfMissing()
            files := DIRECTORY(look)
            for each file in files
           
                fn := file [1]
                db := dir + fn
                missingExt := LEFT(fn, len(fn) -4)
        
                ? "DB:", db
                USE (db) READONLY

                afields := DBSTRUCT()
                hnd := FCREATE("schema"+ HB_PS() + fn + ".txt")
                for each fld in afields
                    FWRITE(hnd, fld[1])
                    FWRITE(hnd, CHR(9))
                    FWRITE(hnd, fld[2])
                    FWRITE(hnd, CHR(9))
                    FWRITE(hnd,STR(fld[3]))
                    FWRITE(hnd, CHR(9))
                    FWRITE(hnd,STR(fld[4]))
                    FWRITE(hnd, CHR(9))
                    FWRITE(hnd, HB_EOL())
                next
                FCLOSE(hnd)
                for each suffix in indexes
                    look2 := dir + missingExt + suffix
                    ifiles := DIRECTORY(look2)
                    for each ifile in ifiles
                        ifn :=  ifile[1]
                        idx := dir + ifn
                        ? "Index:", idx
                        USE (db) READONLY INDEX (idx)
                        ikey := INDEXKEY()
                        hnd := FCREATE("schema" + HB_PS() + ifn + ".txt")
                        FWRITE(hnd, ikey)
                        FWRITE(hnd, HB_EOL())
                        FCLOSE(hnd)
                    next
                next
            next
        next
    next

  

    if .F. 
        ERRORLEVEL(25)
    end if
    quit

