#define MAC( X ) X

namespace MAC(broken) 
{
}


//need this to stop moc giving warning.
class Dummy 
    : public QObject
{
    Q_OBJECT
};

