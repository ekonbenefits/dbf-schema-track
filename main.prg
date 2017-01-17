
#define K_DIR_ARG    "--dir="
#define K_OUT_ARG    "--out="
#define K_VERBOSE_ARG    "--verbose"
#define K_LIST_ARG    "--list="

PROCEDURE CreateStructDirIfMissing(schemaDir)
    path := schemaDir
    IF(!FILE(path))
        MakeDir(schemaDir)
    ENDIF

FUNCTION CheckArgFlag (arg, flag)
    return Lower(Left(arg, Len(flag))) == flag

FUNCTION ParseFlag (arg, flag)
    return SubStr(arg, Len(flag) + 1)

PROCEDURE MAIN(...)
    LOCAL args := HB_AParams()
    LOCAL dirs := {}
    LOCAL prefixes := {}
    LOCAL indexes := {".NT*",".ND*", ".CD*",".MD*"}
    LOCAL schemaDir := "schema"
    LOCAL mapOfHashes := hb_Hash()
    local verbose := .F.
    LOCAL tmp, dir, prefix, file, ifile, fld, entry, suffix, pfile, key

    hb_hSetAutoAdd(mapOfHashes, .T.)

    FOR EACH tmp IN args
        DO CASE
        CASE CheckArgFlag(tmp, K_DIR_ARG)
            AAdd(dirs, ParseFlag(tmp,K_DIR_ARG))
        CASE CheckArgFlag(tmp,K_OUT_ARG)
            schemaDir := ParseFlag(tmp,K_OUT_ARG)
        CASE CheckArgFlag(tmp,K_VERBOSE_ARG)
            verbose := .T.
        CASE CheckArgFlag(tmp, K_LIST_ARG)
            preFilePath := ParseFlag(tmp, K_LIST_ARG)
            HB_FUse(preFilePath)
            DO WHILE !HB_FEOF()
                AAdd(prefixes, HB_FReadAndSkip())
            ENDDO
            HB_FUse()
        OTHERWISE
            AAdd(prefixes, tmp)
        ENDCASE
    NEXT

    if EMPTY(dirs)
      AAdd(dirs, "")
    end if

    CreateStructDirIfMissing(schemaDir)

    priorFiles := DIRECTORY(schemaDir + HB_PS())
    for each pfile in priorFiles
        fileData := ""
        filePath := schemaDir + HB_PS() + pFile[1]
        HB_FUse(filePath)
            DO WHILE !HB_FEOF()
                fileData += HB_FReadAndSkip()
            ENDDO
        HB_FUse()
        fileSha := HB_StrToHex(hb_sha256(fileData))
        mapOfHashes[filePath] := {fileSha, ""}

    next

    for each dir in dirs
        for each prefix in prefixes
            look := dir + prefix + "*.DBF"
            if verbose
                ? "Prefix: ", look
            endif
            files := DIRECTORY(look)
            for each file in files
           
                fn := file [1]
                db := dir + fn
                missingExt := LEFT(fn, len(fn) -4)
                if verbose
                ? "DB:", db
                endif
                USE (db) READONLY

                afields := DBSTRUCT()
                hnd := FCREATE(schemaDir+ HB_PS() + fn + ".txt")
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
                        if verbose
                            ? "Index:", idx
                        endif
                        USE (db) READONLY INDEX (idx)
                        ikey := INDEXKEY()
                        hnd := FCREATE(schemaDir + HB_PS() + ifn + ".txt")
                        FWRITE(hnd, ikey)
                        FWRITE(hnd, HB_EOL())
                        FCLOSE(hnd)
                    next
                next
            next
        next
    next

    priorFiles := DIRECTORY(schemaDir + HB_PS())
    for each pfile in priorFiles
        fileData := ""
        filePath := schemaDir + HB_PS() + pFile[1]
        HB_FUse(filePath)
            DO WHILE !HB_FEOF()
                fileData += HB_FReadAndSkip()
            ENDDO
        HB_FUse()
        fileSha := HB_StrToHex(hb_sha256(fileData))
        if !hb_hHaskey(mapOfHashes, filePath)
            mapOfHashes[filePath] := {"", fileSha}
        else
            priorSha := mapOfHashes[filePath][1]
            mapOfHashes[filePath] := {priorSha, fileSha}
        endif 

    next

    test := .T.
    for each key in hb_hKeys(mapOfHashes)
       if mapOfHashes[key][1] <> mapOfHashes[key][2] 
            test := .F.
            ? key, " schema has changed."
       endif
    next

    if !test 
         ? "Please recommit"
         ERRORLEVEL(25)
    ENDIF
 
    quit

