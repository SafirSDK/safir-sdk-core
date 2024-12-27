#include "typeaheadwidget.h"
#include <qcoreevent.h>
#include <QKeyEvent>
#include <QApplication>
#include <QWindow>
#include <QFontMetrics>

namespace {

QWidget* TopParent(QWidget *w)
{
    auto p = w;
    while (p->parentWidget() != nullptr)
    {
        p =  p->parentWidget();
    }
    return p;
}

// This calculation can be improved. It is based on the assumption that the listView model
// only only has 1 column, and that it uses the application default font.
int CalculateContentWidth(const QSortFilterProxyModel* model)
{
    // Find the widest row in the model
    QFontMetrics fm(qApp->font());
    int maxWidth = 0;
    for (int row = 0; row < model->rowCount(); ++row)
    {
        auto val = model->index(row, 0).data().toString();
        int w = fm.horizontalAdvance(val);
        if (w > maxWidth)
        {
            maxWidth = w;
        }
    }

    return maxWidth;
}

}

TypeAheadWidget::TypeAheadWidget(QWidget *parent)
    : QLineEdit(parent)
    , m_listView(new QListView(TopParent(parent))) // set the top parent for the listView, to make it possible to have it pop up in front of every other widget
{
    setClearButtonEnabled(true);
    m_listView->setVisible(false);
    installEventFilter(this);
    connect(this, &QLineEdit::textEdited, this, &TypeAheadWidget::OnTextEdited);

    connect(m_listView, &QListView::doubleClicked, this, [this](const QModelIndex& ix)
    {
        if (ix.isValid())
        {
            clear();
            m_listView->setVisible(false);
            emit ItemSelected(ix);
        }
    });
}

TypeAheadWidget::~TypeAheadWidget()
{
    delete m_listView;
}

void TypeAheadWidget::SetModel(QSortFilterProxyModel* model)
{
    m_model = model;
    clear();
    m_listView->setVisible(false);
    m_listView->setModel(m_model);

    if (m_model != nullptr && m_model->rowCount() > 0)
    {
        m_dropdownRowWidth = CalculateContentWidth(m_model) * 1.3;
        m_dropdownRowHeight = m_listView->visualRect(m_model->index(0,0)).height();
    }
}

QSortFilterProxyModel* TypeAheadWidget::Model() const
{
    return m_model;
}

void TypeAheadWidget::OnTextEdited(const QString& text)
{
    if (m_model == nullptr)
    {
        return;
    }

    if (text.isEmpty())
    {
        m_listView->setVisible(false);
        return;
    }

    QRegularExpression regex(text, QRegularExpression::CaseInsensitiveOption);
    if (!regex.isValid())
    {
        setStyleSheet("background:red;");
        return;
    }

    m_model->setFilterRegularExpression(std::move(regex));
    setStyleSheet("");

    auto numRows = m_model->rowCount();
    if (numRows == 0)
    {
        return;
    }

    // Calculate the position and size of dropdown listView
    auto pg = parentWidget()->mapToGlobal(pos());
    pg.ry() += size().height() + 2;
    auto pos = m_listView->parentWidget()->mapFromGlobal(pg);
    m_listView->move(pos);

    auto maxHeight = parentWidget()->height() - pos.y() - 20;
    auto neededHeight = numRows * m_dropdownRowHeight + m_dropdownRowHeight / 2;
    auto height = std::min(maxHeight, neededHeight);
    m_listView->setMinimumSize(m_dropdownRowWidth, height);
    m_listView->setMaximumSize(m_dropdownRowWidth, height);

    m_listView->setVisible(true);
    m_listView->raise();
}

bool TypeAheadWidget::eventFilter(QObject *obj, QEvent *event)
{
    if (m_model == nullptr || !m_listView->isVisible() || m_model->rowCount() == 0)
    {
        return QLineEdit::eventFilter(obj, event);
    }

    if(event->type() == QEvent::KeyPress)
    {
        QKeyEvent *keyEvent = static_cast<QKeyEvent*>(event);
        if (keyEvent->key() == Qt::Key_Down)
        {
            // Arrow down pressed. Step current index by one and handle wrap around.
            auto ix = m_listView->currentIndex();
            if (!ix.isValid())
            {
                m_listView->setCurrentIndex(m_model->index(0,0));
                return true;
            }
            else
            {
                auto row = ix.row() == m_model->rowCount() - 1 ? 0 : ix.row() + 1;
                m_listView->setCurrentIndex(m_model->index(row, 0));
                return true;
            }
        }
        else if (keyEvent->key() == Qt::Key_Up)
        {
            // Arrow up pressed. Decrease current index by one and handle wrap around.
            auto ix = m_listView->currentIndex();
            if (!ix.isValid())
            {
                m_listView->setCurrentIndex(m_model->index(m_model->rowCount() - 1, 0));
                return true;
            }
            else
            {
                auto row = ix.row() == 0 ? m_model->rowCount() - 1 : ix.row() - 1;
                m_listView->setCurrentIndex(m_model->index(row, 0));
                return true;
            }
        }
        else if (keyEvent->key() == Qt::Key_Return || keyEvent->key() == Qt::Key_Enter)
        {
            // Return is pressed, use the currentIndex as selected value.
            auto ix = m_listView->currentIndex();
            if (ix.isValid())
            {
                clear();
                m_listView->setVisible(false);
                emit ItemSelected(ix);
            }
        }
    }

    return QLineEdit::eventFilter(obj, event);
}
