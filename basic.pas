BEGIN
    BEGIN
        number := 2;
        a := number;
        b := 10 * a + 10 * number div 4;
        c := a - - b
    END;

    x := 11;
END.

