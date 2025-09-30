/******************************************************************************
* Read-only delegate that lets the user select arbitrary substrings
* inside a table cell (text-editor style) without modifying the model.
******************************************************************************/
#pragma once

#include <QStyledItemDelegate>
#include <QLineEdit>
#include <QHelpEvent>
#include <QToolTip>
#include <QFontMetrics>
#include <QAbstractItemView>
#include <QPainter>

class SelectableTextDelegate : public QStyledItemDelegate
{
    Q_OBJECT
public:
    using QStyledItemDelegate::QStyledItemDelegate;

    QWidget* createEditor(QWidget* parent,
                          const QStyleOptionViewItem&,
                          const QModelIndex&) const override
    {
        auto* le = new QLineEdit(parent);
        le->setReadOnly(true);
        le->setFrame(false);
        le->setStyleSheet("border:0px; margin:0px; spacing:0px; padding:0px; border-radius:0px;");
        // QLineEdit already supports mouse/keyboard selection when read-only.
        // Enable drag so the user can highlight substrings.
        le->setDragEnabled(true);
        return le;
    }

    void setEditorData(QWidget* editor,
                       const QModelIndex& index) const override
    {
        auto* le = qobject_cast<QLineEdit*>(editor);
        le->setText(index.data(Qt::DisplayRole).toString());
    }

    void setModelData(QWidget*, QAbstractItemModel*,
                      const QModelIndex&) const override
    {
        /* read-only – nothing written back */
    }

    void updateEditorGeometry(QWidget* editor,
                              const QStyleOptionViewItem& option,
                              const QModelIndex&) const override
    {
        editor->setGeometry(option.rect);
    }

    // Show a tooltip only when the cell text is clipped by the column width
    bool helpEvent(QHelpEvent* event,
                   QAbstractItemView* view,
                   const QStyleOptionViewItem& option,
                   const QModelIndex& index) override
    {
        if (!event || !view || !index.isValid())
            return false;                       // let default handling try

        const QString text = index.data(Qt::DisplayRole).toString();
        const int     availW = option.rect.width() - 4;       // small margin
        const QFontMetrics fm(option.font);

        const bool clipped = fm.horizontalAdvance(text) > availW;

        if (clipped)
        {
            QToolTip::showText(event->globalPos(), text, view);
        }
        else
        {
            QToolTip::hideText();               // ensure no stale tooltip
        }
        return true;                            // event handled → suppress model tooltip
    }

    // ------------------------------------------------------------------
    //  Paint without the hover flag so the view never draws a hover
    //  background, keeping our highlight colours intact.
    // ------------------------------------------------------------------
    void paint(QPainter* painter,
               const QStyleOptionViewItem& option,
               const QModelIndex& index) const override
    {
        QStyleOptionViewItem opt(option);
        opt.state &= ~QStyle::State_MouseOver;      // remove hover state
        QStyledItemDelegate::paint(painter, opt, index);
    }
};
