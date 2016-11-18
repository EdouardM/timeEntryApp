namespace TimeEntry
    open DomainTypes
    module DataBase =
            //DataBase model (pure)
            type DBTimeRecord =
                {
                    Site        : Site
                    ShopFloor   : ShopFloor
                    WorkCenter  : WorkCenter
                    TimeType    : TimeType
                    Duration    : Duration
                    NbPeople    : NbPeople
                }

            ///Function to convert one Domain Record into records to insert into Database
            let toDB  (time : TimeRecord) = 
                match time.TimeEntry with
                    //List of one record: Machine time
                    | MachineOnly duration -> 
                        [ {   
                            Site        = time.Site 
                            ShopFloor   = time.ShopFloor 
                            WorkCenter  = time.WorkCenter 
                            TimeType    = MachineTime
                            Duration    = duration
                            NbPeople    = NbPeople 0. } ]
                    //List of two records: Machine & Labour time
                    | MachineAndLabour (duration, nb) -> 
                        [ { 
                            Site        = time.Site
                            ShopFloor   = time.ShopFloor 
                            WorkCenter  = time.WorkCenter
                            TimeType    = MachineTime 
                            Duration    = duration
                            NbPeople    = NbPeople 0. } ;

                            { 
                            Site        = time.Site
                            ShopFloor   = time.ShopFloor 
                            WorkCenter  = time.WorkCenter
                            TimeType    = MachineTime 
                            Duration    = duration
                            NbPeople    = nb }]
                    //List of one record: Labour time
                    | LabourOnly (duration, nb) -> 
                        [ {   
                            Site        = time.Site 
                            ShopFloor   = time.ShopFloor
                            WorkCenter  = time.WorkCenter
                            TimeType    = LabourTime
                            Duration    = duration 
                            NbPeople    = nb } ]
