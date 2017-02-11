namespace TimeEntry

module Authorization =
    
    open TimeEntry.DomainTypes
    open TimeEntry.Constructors


    let onlyForSameLogin (login: Login ) (userinfo: UserInfo) (f: Login -> 'a) = 
        if userinfo.Login = login then
            Some f
        else
            None

    let onlyForAdmin (user: UserInfo) (f: 'a -> 'b) = 
        if user.Level = Admin then 
            Some f
        else
            None 

    let notForViewer (user: UserInfo) (f: 'a -> 'b) = 
        if user.Level <> Viewer then
            Some f
        else 
            None

    ///Checks if user has some authorized sites before using function f on them
    let hasAuthSites (userinfo : UserInfo) (f: SiteAccess -> 'a) = 
        match userinfo.SiteAccess with
            | AllSites      -> Some f
            | SiteList []   -> None
            | SiteList ls   -> Some f 


    ///Checks if user has an authorized access to one site before using function f on it
    let onlyForAuthSites (site: Site) (userinfo: UserInfo) (f: Site -> 'a) = 
        match userinfo.SiteAccess with
            | AllSites -> Some f
            | SiteList sites ->
                if List.contains site sites then
                    Some f
                else None
